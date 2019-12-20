package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.scalafromjs.ConvertProject._
import com.github.opengrabeso.scalafromjs.esprima._
import SymbolTypes._
import TypesRule._

/**
  * Code responsible for parsing d.ts files and matching them to the main project AST
  */

object TypesRule {
  def transform(n: NodeExtended): NodeExtended = {
    val rules = n.config.collectRules[TypesRule]
    rules.foldLeft(n)((c, r) => r.provideTypes(c))
  }

  def loadSymbols(code: String) = {
    // create a separate project for it, the result is the types only
    val ast = parse(code, true)
    // scan all global symbols, esp. exported ones
    val symbols = Map.newBuilder[String, Node.Node]

    def handleExport() = false
    def handleDeclaration(node: Node.Node, name: String) = {
      symbols += name -> node
      true
    }
    ast.walkWithScope {(node, context) =>
      node match {
        case _: Node.ExportAllDeclaration => // enter into any export (this is still top-level)
          handleExport()
        case _: Node.ExportNamedDeclaration =>
          handleExport()
        case _: Node.ExportDefaultDeclaration =>
          handleExport()

        case VarDecl(name, _, _) =>
          handleDeclaration(node, name)
        // find a corresponding global symbol in the d.ts types
        case Node.FunctionDeclaration(Node.Identifier(name), params, body, generator, ret) =>
          handleDeclaration(node, name)
        case Node.FunctionExpression(Node.Identifier(name), params, body, generator, ret) =>
          handleDeclaration(node, name)
        case Node.ClassDeclaration(Node.Identifier(name), params, body) =>
          handleDeclaration(node, name)
        case Node.ClassExpression(Node.Identifier(name), params, body) =>
          handleDeclaration(node, name)

        case `ast` => // enter the top level
          false
        case _ => // do not enter any other scope
          true
      }
    }

    symbols.result()
  }

  def typeFromIdentifierName(name: String)(context: symbols.ScopeContext) = {
    val t = name match {
      case "number" => SimpleType("Double")
      case "string" => SimpleType("String")
      case "boolean" => SimpleType("Boolean")
      case "any" => SimpleType("Any")
      case "void" => SimpleType("Unit")
      case _ => ClassType(context.findSymId(name))
    }
    t
  }

  def typeFromAST(tpe: Node.TypeAnnotation)(context: symbols.ScopeContext): Option[TypeDesc] = {
    tpe match {
      case Node.TypeName(Node.Identifier(name)) =>
        Some(typeFromIdentifierName(name)(context))
      case Node.TypeReference(tpe, genType) =>
        typeFromAST(tpe)(context) // TODO: represent generics in TypeInfo
      case Node.ArrayType(item) =>
        Some(ArrayType(typeFromAST(item)(context).getOrElse(AnyType)))
      case _ =>
        None
    }
  }

  def typeInfoFromAST(tpe: Node.TypeAnnotation)(context: symbols.ScopeContext): Option[TypeInfo] = {
    typeFromAST(tpe)(context).map(TypeInfo.both(_).copy(certain = true))
  }
}
case class TypesRule(types: String, root: String) extends ExternalRule {
  // load the d.ts
  val dtsSymbols = {
    val project = ConvertProject.loadControlFile(PathUtils.resolveSibling(root, types))
    loadSymbols(project.code)
  }

  // we cannot use normal apply, as we need to be called at a specific stage
  def provideTypes(n: NodeExtended): NodeExtended = {
    // walk the AST, search for types for any global symbols

    // list top-level classes only
    val classList = new Classes.ClassListHarmony(n.top, false)

    var types = n.types.types
    val ast = n.top

    ast.walkWithScope { (node, context) =>
      def handleParameterTypes(astPars: Seq[Node.FunctionParameter], dtsPars: Seq[Node.FunctionParameter])(scopeId: symbols.ScopeContext.ScopeId) = {
        // match parameters by position, their names may differ
        for {
          (
            Transform.ParName(pjs),
            Node.FunctionParameterWithType(_, Defined(t), defValue, optional)
          ) <- astPars zip dtsPars
          tt <- typeInfoFromAST(t)(context)
        } {
          types += symbols.SymId(pjs, scopeId) -> tt
        }
      }

      def handleVar(node: Node.Node, name: String) = {
        for {
          s@VarDecl(_, _, _) <- dtsSymbols.get(name)
          t <- Option(s.declarations.head.`type`)
          tt <- typeInfoFromAST(t)(context)
        } {
          types += context.findSymId(name) -> tt
        }
        true
      }
      def handleFunction(node: Node.Node, params: Seq[Node.FunctionParameter], name: String) = {
        for (AnyFunEx(pars, tpe, body) <- dtsSymbols.get(name)) {
          // we have already entered the function scope, parameters can be found in localSymbols
          handleParameterTypes(params, pars)(symbols.ScopeContext.getNodeId(node))
          for {
            t <- tpe
            tt <- typeInfoFromAST(t)(context)
          } {
            types += context.findSymId(name) -> tt
          }
        }
        true
      }
      def handleClass(node: Node.ClassDeclaration, name: String) = {
        for {
          Node.ClassDeclaration(_, superClass, Defined(b)) <- dtsSymbols.get(name)
          clsSym = context.findSymId(name)
          if !clsSym.isGlobal // global means not defined in the AST we are traversing
          clsNode <- classList.get(clsSym)
          clsId = symbols.ScopeContext.getNodeId(clsNode.body)
          member <- b.body
        } {
          member match {
            // list d.ts members, create type information for corresponding symbols
            case Node.MethodDefinition(Node.Identifier(funName), ret, _, AnyFunEx(dtsPars, retFun, body), _, _) => // member function
              for {
                t <- Option(ret).orElse(retFun)
                tt <- typeInfoFromAST(t)(context)
              } {
                types += SymbolMapId(funName, clsId) -> tt
              }
              // to create parameter types we need to find the AST method definition node
              def findMethod(funName: String) = {
                // if the method is a constructor, we need a special handling for variables as well
                // an alternative could be to perform TS types handling after convertProtoClassesRecursive
                // (before any constructor transformations)
                if (funName == "constructor") Classes.findMethod(node, funName).toSeq ++ Classes.findInlineBody(node)
                else Classes.findMethod(node, funName).toSeq
              }
              for {
                astMethod <- findMethod(funName) // TODO: handle overloads
                methodId = symbols.ScopeContext.getNodeId(astMethod.value)
                Node.MethodDefinition(_, _, _, AnyFun(astPars, _), _, _) = astMethod
              } {
                handleParameterTypes(astPars, dtsPars)(methodId)
              }

            case Node.MethodDefinition(Node.Identifier(funName), Defined(ret), _, value, _, _) => // plain member with known type
              for (tt <- typeInfoFromAST(ret)(context)) {
                types += SymbolMapId(funName, clsId) -> tt
              }
          }
        }
        true
      }
      node match {
        case VarDecl(name, _, _) =>
          handleVar(node, name)
        case Node.FunctionDeclaration(Node.Identifier(name), params, body, generator, ret) =>
          handleFunction(node, params, name)
        case Node.FunctionExpression(Node.Identifier(name), params, body, generator, ret) =>
          handleFunction(node, params, name)
        case c@Node.ClassDeclaration(Node.Identifier(name), params, body) =>
          handleClass(c, name)
         // Node.ClassExpression(Node.Identifier(name), params, body) => name is ClassExpression is local only - it makes to sense to process it
        case `ast` =>
          false
        case _ =>
          true

      }
    }
    n.copy(types = n.types.copy(types = types))
  }
}
