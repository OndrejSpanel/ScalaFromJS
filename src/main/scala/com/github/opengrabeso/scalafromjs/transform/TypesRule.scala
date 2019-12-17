package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.scalafromjs.ConvertProject._
import com.github.opengrabeso.scalafromjs.esprima._
import SymbolTypes._
import TypesRule._

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

  def typeFromAST(tpe: Node.TypeAnnotation)(context: symbols.ScopeContext): TypeDesc = {
    tpe match {
      case Node.TypeName(Node.Identifier(name)) =>
        name match {
          case "number" => SimpleType("Double")
          case "string" => SimpleType("String")
          case "bool" => SimpleType("Boolean")
          case "any" => SimpleType("Any")
          case "void" => SimpleType("Unit")
          case _ => ClassType(context.findSymId(name))
        }
      case _ =>
        AnyType
    }
  }

  def typeInfoFromAST(tpe: Node.TypeAnnotation)(context: symbols.ScopeContext): TypeInfo = {
    TypeInfo.both(typeFromAST(tpe)(context)).copy(certain = true)
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
    val top = n.top
    top.walkWithScope { (node, context) =>
      def handleVar(node: Node.Node, name: String) = {
        for {
          s@VarDecl(_, _, _) <- dtsSymbols.get(name)
          t <- Option(s.declarations.head.`type`)
        } {
          types += context.findSymId(name) -> TypeInfo.both(typeFromAST(t)(context))
        }
        true
      }
      def handleFunction(node: Node.Node, params: Seq[Node.FunctionParameter], name: String) = {
        for (AnyFunEx(pars, tpe, body) <- dtsSymbols.get(name)) {
          // we have already entered the function scope, parameters can be found in localSymbols
          // match parameters by position, their names may differ
          for {
            (
              Transform.ParName(pjs),
              Node.FunctionParameterWithType(_, Defined(t), defValue, optional)
            ) <- params zip pars
          } {
            types += context.findSymId(pjs) -> typeInfoFromAST(t)(context)
          }
          for (t <- tpe) {
            types += context.findSymId(name) -> typeInfoFromAST(t)(context)
          }
        }
        true
      }
      def handleClass(node: Node.Node, name: String) = {
        for {
          Node.ClassDeclaration(_, s, b) <- dtsSymbols.get(name)
          clsSym = context.findSymId(name)
          if !clsSym.isGlobal // global means not defined in the "top" we are traversing
          clsNode <- classList.get(clsSym)
          clsId = symbols.ScopeContext.getNodeId(clsNode.body)
          member <- b.body
        } {
          member match { // list d.ts members, create type information for corresponding
            case Node.MethodDefinition(Node.Identifier(funName), ret, _, AnyFunEx(params, retFun, body), _, _) => // member function
              for (t <- Option(ret).orElse(retFun)) {
                types += SymbolMapId(funName, clsId) -> typeInfoFromAST(t)(context)
              }
            case Node.MethodDefinition(Node.Identifier(funName), Defined(ret), _, value, _, _) => // plain member with known type
              types += SymbolMapId(funName, clsId) -> typeInfoFromAST(ret)(context)
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
        case Node.ClassDeclaration(Node.Identifier(name), params, body) =>
          handleClass(node, name)
         // Node.ClassExpression(Node.Identifier(name), params, body) => name is ClassExpression is local only - it makes to sense to process it
        case `top` =>
          false
        case _ =>
          true

      }
    }
    n.copy(types = n.types.copy(types = types))
  }
}
