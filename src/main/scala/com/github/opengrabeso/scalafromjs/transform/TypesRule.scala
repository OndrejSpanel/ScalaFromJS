package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.ConvertProject._
import com.github.opengrabeso.scalafromjs.esprima._
import TypesRule._
import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.esprima.Node.{ArrayType => _, FunctionType => _, _}
import SymbolTypes._

import scala.collection.mutable

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
    val symbols = Map.newBuilder[String, Node]

    def handleExport() = false
    def handleDeclaration(node: Node, name: String) = {
      symbols += name -> node
      true
    }
    ast.walkWithScope {(node, context) =>
      node match {
        case _: ExportAllDeclaration => // enter into any export (this is still top-level)
          handleExport()
        case _: ExportNamedDeclaration =>
          handleExport()
        case _: ExportDefaultDeclaration =>
          handleExport()

        case VarDecl(name, _, _) =>
          handleDeclaration(node, name)
        // find a corresponding global symbol in the d.ts types
        case FunctionDeclaration(Identifier(name), params, body, generator, ret) =>
          handleDeclaration(node, name)
        case FunctionExpression(Identifier(name), params, body, generator, ret) =>
          handleDeclaration(node, name)
        case ClassDeclaration(Identifier(name), params, body) =>
          handleDeclaration(node, name)
        case ClassExpression(Identifier(name), params, body) =>
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
      case "number" => Some(SimpleType("Double"))
      case "string" => Some(SimpleType("String"))
      case "boolean" => Some(SimpleType("Boolean"))
      case "any" => Some(SimpleType("Any"))
      case "void" => Some(SimpleType("Unit"))
      case "this" => None // TODO: some better support for this type
      case _ => Some(ClassType(context.findSymId(name)))
    }
    t
  }

  def typeFromAST(tpe: TypeAnnotation)(context: symbols.ScopeContext): Option[TypeDesc] = {
    tpe match {
      case TypeName(Identifier(name)) =>
        typeFromIdentifierName(name)(context)
      case TypeReference(tpe, genType) =>
        typeFromAST(tpe)(context) // TODO: represent generics in TypeInfo
      case Node.ArrayType(item) =>
        Some(ArrayType(typeFromAST(item)(context).getOrElse(AnyType)))
      case ObjectType(Seq(TypeMember(null, _, t))) => // object containing only index signature, like {[i: number]: t}
        typeFromAST(t)(context).map(MapType)
      case ObjectType(body) =>
        None // TODO: can be converted to structural typing
      case Node.FunctionType(pars, retType) =>
        val retT = typeFromAST(retType)(context).getOrElse(AnyType)
        val parsT = pars.map { p =>
          Transform.typeFromPar(p).flatMap(typeFromAST(_)(context)).getOrElse(AnyType)
        }
        Some(FunctionType(retT, parsT.toArray[TypeDesc]))
      case _ =>
        None
    }
  }

  def typeInfoFromAST(tpe: TypeAnnotation)(context: symbols.ScopeContext): Option[TypeInfo] = {
    typeFromAST(tpe)(context).map(TypeInfo.both(_).copy(certain = true))
  }
}
case class TypesRule(types: String, root: String) extends ExternalRule {
  val project = ConvertProject.loadControlFile(PathUtils.resolveSibling(root, types))
  // load the d.ts
  val dtsSymbols = loadSymbols(project.code)

  // we cannot use normal apply, as we need to be called at a specific stage
  def provideTypes(n: NodeExtended): NodeExtended = {
    // walk the AST, search for types for any global symbols

    // list top-level classes only
    val classList = new Classes.ClassListHarmony(n.top, false)

    var types = n.types.types
    val ast = n.top

    ast.walkWithScope { (node, context) =>
      def getParameterTypes(dtsPars: Seq[FunctionParameter])(scopeId: symbols.ScopeContext.ScopeId) = {
        // match parameters by position, their names may differ
        for {
          FunctionParameterWithType(_, Defined(t), defValue, optional) <- dtsPars
          tt = typeInfoFromAST(t)(context)
        } yield {
          tt
        }
      }

      def handleParameterTypes(astPars: Seq[FunctionParameter], dtsPars: Seq[FunctionParameter])(scopeId: symbols.ScopeContext.ScopeId) = {
        // match parameters by position, their names may differ
        val parNames = astPars.map(Transform.nameFromPar)
        val parTypes = getParameterTypes(dtsPars)(scopeId)
        for {
          (Some(pjs), Some(tt)) <- parNames zip parTypes
        } {
          types += symbols.SymId(pjs, scopeId) -> tt
        }
      }

      def handleVar(node: Node, name: String) = {
        for {
          s@VarDecl(_, _, _) <- dtsSymbols.get(name)
          t <- Option(s.declarations.head.`type`)
          tt <- typeInfoFromAST(t)(context)
        } {
          types += context.findSymId(name) -> tt
        }
        true
      }
      def handleFunction(node: Node, params: Seq[FunctionParameter], name: String) = {
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
      def handleClass(node: ClassDeclaration, name: String) = {
        for {
          ClassDeclaration(_, superClass, Defined(b)) <- dtsSymbols.get(name)
          clsSym = context.findSymId(name)
          if !clsSym.isGlobal // global means not defined in the AST we are traversing
          clsNode <- classList.get(clsSym)
          clsId = symbols.ScopeContext.getNodeId(clsNode.body)
          member <- b.body
        } {
          member match {
            // list d.ts members, create type information for corresponding symbols
            case MethodDefinition(Identifier(funName), ret, _, AnyFunEx(dtsPars, retFun, body), _, static) => // member function
              // to create parameter types we need to find the AST method definition node
              def findMethod() = {  // TODO: handle overloads
                // if the method is a constructor, we need a special handling for variables as well
                // an alternative could be to perform TS types handling after convertProtoClassesRecursive
                // (before any constructor transformations)
                if (funName == "constructor") Classes.findMethod(node, funName, static).toSeq ++ Classes.findInlineBody(node)
                else Classes.findMethod(node, funName, static).toSeq
              }
              for {
                astMethod <- findMethod()
                methodId = symbols.ScopeContext.getNodeId(astMethod.value)
                MethodDefinition(_, _, _, AnyFun(astPars, _), _, _) = astMethod
                t = Option(ret).orElse(retFun)
                tt = t.flatMap(typeFromAST(_)(context)).getOrElse(AnyType)
              } {
                val parTypes = getParameterTypes(dtsPars)(methodId)
                handleParameterTypes(astPars, dtsPars)(methodId)
                FunctionType(tt, parTypes.map(_.map(_.declType).getOrElse(AnyType)).toArray[TypeDesc])
              }

            case MethodDefinition(Identifier(funName), Defined(ret), _, value, _, _) => // plain member with known type
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
        case FunctionDeclaration(Identifier(name), params, body, generator, ret) =>
          handleFunction(node, params, name)
        case FunctionExpression(Identifier(name), params, body, generator, ret) =>
          handleFunction(node, params, name)
        case c@ClassDeclaration(Identifier(name), params, body) =>
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
