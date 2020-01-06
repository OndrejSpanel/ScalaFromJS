package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.ConvertProject._
import com.github.opengrabeso.scalafromjs.esprima._
import TypesRule._
import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.esprima.Node.{ArrayType => _, FunctionType => _, _}
import SymbolTypes._
import com.github.opengrabeso.scalafromjs

import scala.collection.mutable
import scala.util.Try

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
        case ClassDeclaration(Identifier(name), parent, interfaces, params, body) =>
          handleDeclaration(node, name)
        case NamespaceDeclaration(Identifier(name), body) =>
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

  def typeFromIdentifierName(name: String)(context: symbols.ScopeContext): Option[TypeDesc] = {
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

  def typeFromLiteral(raw: String): Option[TypeDesc] = raw match {
    case "true" | "false" => Some(SimpleType("Boolean"))
    case s if Try(s.toDouble).isSuccess || Try(s.toInt).isSuccess => Some(SimpleType("Double"))
    case s if s.head == '"' && s.last == '"' || s.head == '\'' && s.last == '\'' => Some(SimpleType("String"))
    case "null" => None
    case _ => None
  }

  def typeFromAST(tpe: TypeAnnotation)(context: symbols.ScopeContext): Option[TypeDesc] = {
    tpe match {
      case LiteralType(Literal(_, raw)) =>
        typeFromLiteral(raw)
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
    val classList = Classes.ClassListHarmony.fromAST(n.top, false)

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

      /**
        *
        * @param node d.ts node
        * @param name name of the dst node (same as node.id.name)
        * @param astVarNode js node
        */
      def handleNamespace(node: NamespaceDeclaration, name: String, astVarId: symbols.SymId, astVarNode: VariableDeclaration) = {
        // convert to a class with static members
        object ExtractExport {
          def unapply(arg: ExportDeclaration): Option[ExportableNamedDeclaration] = {
            arg match {
              case e: ExportNamedDeclaration =>
                Some(e.declaration)
              case d: ExportableNamedDeclaration =>
                Some(d)
              case _ =>
                None
            }
          }
        }
        // TODO: we need a class body to scope the symbol properly, we have VariableDeclaration only
        // a dedicated transformation would be necessary for this
        val clsId = symbols.ScopeContext.getNodeId(node.body)
        context.withScope(node.body) {
          for (ExtractExport(member) <- node.body.body) {
            member match {
              case FunctionDeclaration(Identifier(funName), dtsPars, _, _, ret) => // member function
                /*
                val tt = typeFromAST(ret)(context)
                // TODO: find a js member corresponding to funName
                astVarNode.walk
                //val methodId = SymbolMapId(funName, clsId)
                val parTypes = getParameterTypes(dtsPars)(astVarId)
                // TODO: astPars from astNode
                //handleParameterTypes(astPars, dtsPars)(methodId)
                val parTypesDesc = parTypes.map(_.map(_.declType).getOrElse(AnyType)).toArray[TypeDesc]
                types += SymbolMapId(funName, clsId) -> TypeInfo.both(FunctionType(tt, parTypesDesc)).copy(certain = true)
                 */

              case VarDeclTyped(name, _, _, Some(tpe)) => // plain member with known type
                for (tt <- typeInfoFromAST(tpe)(context)) {
                  types += SymbolMapId(name, clsId) -> tt
                }
              case _ =>

            }
          }
        }
      }
      def handleVar(node: Node, name: String) = {
        // the variable may correspond to a namespace
        for (s <- dtsSymbols.get(name)) {
          (s, node) match {
            case (v: VariableDeclaration, _) =>
              for {
                t <- Option(v.declarations.head.`type`)
                tt <- typeInfoFromAST(t)(context)
              } {
                types += context.findSymId(name) -> tt
              }
            case (n: NamespaceDeclaration, vd@VarDeclTyped(varName, _, _, _)) =>
              val astVarId = context.findSymId(varName)
              handleNamespace(n, n.id.name, astVarId, vd)
            case _ =>
          }
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
        // process parent
        for {
          ClassDeclaration(_, superClass, moreParents, Defined(b), kind) <- dtsSymbols.get(name)
          clsSym = context.findSymId(name)
          if !clsSym.isGlobal // global means not defined in the AST we are traversing
          clsNode <- classList.get(clsSym)
          clsId = symbols.ScopeContext.getNodeId(clsNode.body)
        } {
          for (firstParent <- Option(superClass).orElse(moreParents.headOption)) {
            // TODO: avoid mutating original AST
            // TODO: handle more parents
            if (node.superClass == null) {
              node.superClass = firstParent
            }
          }

          for (member <- b.body) {
            member match {
              // list d.ts members, create type information for corresponding symbols
              case MethodDefinition(Identifier(funName), ret, _, AnyFunEx(dtsPars, retFun, body), _, static) => // member function
                // to create parameter types we need to find the AST method definition node
                def findMethod() = { // TODO: handle overloads
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
                  val parTypesDesc = parTypes.map(_.map(_.declType).getOrElse(AnyType)).toArray[TypeDesc]
                  types += SymbolMapId(funName, clsId) -> TypeInfo.both(FunctionType(tt, parTypesDesc)).copy(certain = true)
                }

              case MethodDefinition(Identifier(funName), Defined(ret), _, value, _, _) => // plain member with known type
                for (tt <- typeInfoFromAST(ret)(context)) {
                  types += SymbolMapId(funName, clsId) -> tt
                }
              case _ =>
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
        case c@ClassDeclaration(Identifier(name), superClass, moreParents, body, _) =>
          handleClass(c, name)
        case `ast` =>
          false
        case _ =>
          true

      }
    }
    n.copy(types = n.types.copy(types = types))
  }
}
