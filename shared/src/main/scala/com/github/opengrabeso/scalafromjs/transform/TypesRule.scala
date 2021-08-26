package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.ConvertProject._
import com.github.opengrabeso.scalafromjs.esprima._
import TypesRule._
import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.esprima.OrType
import com.github.opengrabeso.esprima.Node.{ArrayType => _, FunctionType => _, _}
import SymbolTypes._
import com.github.opengrabeso.scalafromjs.esprima.symbols.Id

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try
import scala.collection.Seq

/**
  * Code responsible for parsing d.ts files and matching them to the main project AST
  */

object TypesRule {

  def transform(n: NodeExtended): NodeExtended = {
    val rules = n.config.collectRules[TypesRule]
    rules.foldLeft(n)((c, r) => r.provideTypes(c))
  }

  def transformEnums(n: NodeExtended): NodeExtended = {
    val rules = n.config.collectRules[TypesRule]
    rules.foldLeft(n)((c, r) => r.transformEnums(c))
  }

  def countTypes(node: Node): Int = {
    var count = 0
    node.walk {
      case _: TypeAnnotation =>
        count += 1
        false
      case _ =>
        false
    }
    count
  }

  def loadSymbols(code: String) = {
    // create a separate project for it, the result is the types only
    val ast = parse(code, true)
    // scan all global symbols, esp. exported ones
    var symbols = Map.empty[String, Node]

    def handleExport() = false
    def handleDeclaration(node: Node, name: String): Boolean = {
      symbols.get(name).map { nodeOld =>
        // if the symbol already exists, choose the more useful one
        if (countTypes(node) > countTypes(nodeOld)) {
          symbols += name -> node
        }
      }.getOrElse {
        symbols += name -> node
      }
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
        case VarDecl(name, _, _) => // ignore variables without a type
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
        case EnumDeclaration(Identifier(name), body) =>
          handleDeclaration(node, name)
        case TypeAliasDeclaration(Identifier(name), tpe) =>
          handleDeclaration(node, name)

        case `ast` => // enter the top level
          false
        case _ => // do not enter any other scope
          true
      }
    }

    symbols
  }

  def typeFromIdentifierName(name: String, memberFun: Boolean)(context: symbols.ScopeContext): Option[TypeDesc] = {
    val t = name match {
      case "number" => Some(SimpleType("Double"))
      case "string" => Some(SimpleType("String"))
      case "boolean" => Some(SimpleType("Boolean"))
      case "any" => None // any is usually a placeholder for missing information
      case "void" => Some(NoType)
      case "this" => None // TODO: some better support for this type
      case _ => Some(ClassType(context.findSymId(name, memberFun)))
    }
    t
  }

  def typeFromLiteral(raw: String): Option[TypeDesc] = raw match {
    case "true" | "false" => Some(SimpleType("Boolean"))
    case s if Try(s.toDouble).isSuccess || Try(s.toInt).isSuccess => Some(SimpleType("Double"))
    case s if s.head == '"' && s.last == '"' || s.head == '\'' && s.last == '\'' => Some(SimpleType("String"))
    case "null" => Some(NullType)
    case _ => None
  }

  def typeFromAST(tpe: TypeAnnotation)(implicit context: symbols.ScopeContext): Option[TypeDesc] = {
    tpe match {
      case LiteralType(Literal(_, raw)) =>
        typeFromLiteral(raw)
      case TypeName(Seq(Identifier(name))) =>
        typeFromIdentifierName(name, false)(context)
      case TypeName(names) =>
        Some(ClassTypeEx(names.init.map(_.name), Id(names.last)))
      case TypeReference(TypeName(Seq(Identifier("Array"))), Seq(genType)) =>
        val typePar = typeFromAST(genType)(context).getOrElse(AnyType)
        Some(ArrayType(typePar))
      case TypeReference(TypeName(names), genType) =>
        val typeNameId = context.findSymId(names.last.name, false)
        val typePar = genType.map(typeFromAST(_)(context).getOrElse(AnyType))
        Some(ClassTypeEx(names.init.map(_.name), typeNameId, typePar))
      case Node.ArrayType(item) =>
        Some(ArrayType(typeFromAST(item)(context).getOrElse(AnyType)))
      case ObjectType(Seq(TypeMember(null, _, _, t))) => // object containing only index signature, like {[i: number]: t}
        typeFromAST(t)(context).map(MapType)
      case ObjectType(body) =>
        None // TODO: can be converted to structural typing
      case Node.FunctionType(pars, retType) =>
        val retT = typeFromAST(retType)(context).getOrElse(AnyType)
        val parsT = pars.map { p =>
          Transform.typeFromPar(p).flatMap(typeFromAST(_)(context)).getOrElse(AnyType)
        }
        Some(FunctionType(retT, parsT.toArray[TypeDesc]))
      case Node.UnionType(left, right) =>
        val l = typeFromAST(left).getOrElse(AnyType)
        val r = typeFromAST(right).getOrElse(AnyType)
        Some(typeUnion(l, r)(ClassOpsUnion))
      case Node.ConditionalType(t, cond, left, right) =>
        // we do not want to parse the parameters, we rather create a union of the types in question
        // note: one of them will frequently be a generic - we currently do not know them
        val l = typeFromAST(left).getOrElse(AnyType)
        val r = typeFromAST(right).getOrElse(AnyType)
        Some(typeUnion(l, r)(ClassOpsUnion))
      case Node.IntersectionType(left, right) =>
        val l = typeFromAST(left).getOrElse(AnyType)
        val r = typeFromAST(right).getOrElse(AnyType)
        Some(typeIntersect(l, r)(ClassOpsUnion))
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
  val enums = dtsSymbols.collect {
    case (k, v: EnumDeclaration) =>
      (k, v)
  }
  val enumValues = enums.values.flatMap { e =>
    e.body.body.map { ev =>
      ev.name.name -> e
    }
  }.toMap

  // WIP: avoid any type manipulation here, should be done later, while scanning the AST
  // we cannot use normal apply, as we need to be called at a specific stage
  def provideTypes(n: NodeExtended): NodeExtended = {
    // walk the AST, search for types for any global symbols

    // list top-level classes only
    val classList = Classes.ClassListHarmony.fromAST(n.top, false)

    var types = n.types

    // scan classes present in the JS
    val classes = Classes.ClassListHarmony.fromAST(n.top, innerClasses = false).classes.map { case (k, v) =>
      k.name
    }.toSet

    // scan objects (variables) present in the JS
    val objects = {
      val objectBuilder = Set.newBuilder[String]
      n.top.walkWithScope { (node, ctx) =>
        node match {
          case Node.VariableDeclarator(Identifier(name), oe: ObjectExpression, _) =>
            objectBuilder += name
            false
          case _ =>
            false
        }
      }
      objectBuilder.result()
    }

    // now have the symbols which are not present in JS, like interfaces, enums or type aliases

    // gather symbols from each DTS which need to be added (have no JS counterpart)
    val symbolsToInclude = project.items.values.zipWithIndex.map {case (item, itemIndex) =>
      if (item.included) {
        val itemRange = (project.offsets(itemIndex), project.offsets(itemIndex + 1))
        // find code corresponding to the offset
        val itemSymbols = dtsSymbols.filter { case (s, node) =>
          // check if node is in the proper range
          node.range._1 >= itemRange._1 && node.range._2 <= itemRange._2
        }
        // any class which does not have a corresponding js definition should be included
        // we have toplevel symbols only, match only by name, sym id not available for d.ts symbols
        item.fullName -> itemSymbols.values.collect {
          case cls@Node.ClassDeclaration(Identifier(name), _, _, _, kind) if !classes.contains(name) && kind != "namespace" =>
            // namespace should always be represented as an object
            cls
          case e@Node.EnumDeclaration(Identifier(name), _) if !objects.contains(name) =>
            e
          case e@Node.TypeAliasDeclaration(Identifier(name), _) if !objects.contains(name) =>
            e
        }
      } else {
        item.fullName -> Nil
      }
    }.toMap


    val beginMarker = ConvertProject.prefixName + "begin"

    // scan the top-level body for the markers, and insert the new content as needed
    // do this before adding types for the symbols, as that should handle the newly added ones as well
    // markers may have uniqye postfix - see declaredGlobal
    val newBody = n.top.body.flatMap {
      case node@Node.ExportNamedDeclaration(VarDecl(name, Some(Node.Literal(value, raw)), "const"), Seq(), null) if name.startsWith(beginMarker) =>
        // we want to insert declarations somewhere after the "begin" markers
        // TODO: insert after imports (if there are any)
        val dtsFromJS = value.replaceAll(".js$", ".d.ts")
        symbolsToInclude.get(dtsFromJS) match {
          case Some(injectSymbols) =>
            val symbolsInJS = injectSymbols.toSeq.map(_.withTokensDeep(node))
            node +: symbolsInJS
          case None =>
            Seq(node)
        }

      case node =>
        Seq(node)

    }

    val newTop = n.top.cloneNode()
    newTop.body = newBody

    newTop.walkWithScope { (node, context) =>
      implicit val ctx = context

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
        for {
          Seq(VariableDeclarator(_, oe: ObjectExpression, _)) <- Option(astVarNode.declarations)
        } {
          val astClsId = symbols.ScopeContext.getNodeId(oe)
          context.withScope(node.body) {
            for (ExtractExport(member) <- node.body.body) {
              member match {
                case FunctionDeclaration(Identifier(funName), dtsPars, body, _, ret) => // member function
                  for {
                    Seq(VariableDeclarator(_, oe: ObjectExpression, _)) <- Option(astVarNode.declarations)
                    fun@AnyFun(astPars, astBody) <- Classes.findObjectMethod(oe, funName)
                    tt = typeFromAST(ret)(context)
                  } {
                    val symId = SymbolMapId(funName, astClsId)
                    // cannot use member, may be in D.TS AST, not in the JS one
                    types = types.handleParameterTypes(symId, tt, astPars, dtsPars, symbols.ScopeContext.getNodeId(fun))(context)
                  }

                case VarDeclTyped(name, _, _, Some(tpe)) => // plain member with known type
                  for (tt <- typeInfoFromAST(tpe)(context)) {
                    types = types add SymbolMapId(name, astClsId) -> tt
                  }
                case _ =>

              }
            }
          }
        }
      }
      def handleEnumValue(vd: VariableDeclarator): Boolean = {
        vd match {
          case VariableDeclarator(Identifier(name), Literal(OrType(_: Double), _), _) =>
            dtsSymbols.get(name).exists {
              case vd: VariableDeclaration =>
                vd.declarations.exists {
                  case VariableDeclarator(Identifier(name), _, Defined(TypeName(Seq(Identifier(tpe))))) if enums.contains(tpe) =>
                    // TODO: proper scoped type (source global, not JS global)
                    val enumType = ClassType(SymbolMapId(tpe, (-1, -1)))
                    types = types add context.findSymId(name, false) -> TypeInfo.both(enumType).copy(certain = true)
                    true
                  case _ =>
                    false
                }
              case _ =>
                false
            } || enumValues.get(name).exists { e =>
              // TODO: proper scoped type (source global, not JS global)
              val enumType = ClassType(SymbolMapId(e.name.name, (-1, -1)))
              types = types add context.findSymId(name, false) -> TypeInfo.both(enumType).copy(certain = true)
              true
            }
          case _ =>
            false
        }
      }

      def handleVar(node: Node, name: String) = {
        // the variable may correspond to a namespace
        val isEnum = node match {
          case v: VariableDeclaration =>
            handleEnumValue(v.declarations.head)
          case _ =>
            false
        }
        if (!isEnum) for (s <- dtsSymbols.get(name)) {
          (s, node) match {
            case (v: VariableDeclaration, _) =>
              for {
                t <- Option(v.declarations.head.`type`)
                tt <- typeInfoFromAST(t)(context)
              } {
                types = types add context.findSymId(name, false) -> tt
                if (watched(name)) println(s"Set from d.ts ${context.findSymId(name, false)} $tt")
              }
            case (n: NamespaceDeclaration, vd@VarDeclTyped(varName, _, _, _)) =>
              val astVarId = context.findSymId(varName, false)
              handleNamespace(n, n.id.name, astVarId, vd)
            case _ =>
          }
        }
        true
      }
      def handleFunction(funSymId: SymId, params: Seq[FunctionParameter], paramScopeId: ScopeId) = {
        for (funEx@AnyFunEx(pars, tpe, body) <- dtsSymbols.get(funSymId.name)) {
          // we have already entered the function scope, parameters can be found in localSymbols
          val tt = tpe.flatMap(typeFromAST(_)(context))
          types = types.handleParameterTypes(funSymId, tt, params, pars, paramScopeId)(context)
        }
        true
      }
      def handleClass(node: ClassDeclaration, name: String) = {
        // process parent
        for {
          ClassDeclarationEx(_, typePars, superClass, moreParents, Defined(b), kind) <- dtsSymbols.get(name)
          clsSym = context.findSymId(name, false)
          if !clsSym.isGlobal // global means not defined in the AST we are traversing
          clsNode <- classList.get(clsSym)
          clsId = symbols.ScopeContext.getNodeId(clsNode.body)
        } {
          // TODO: avoid mutating original AST
          for (firstParent <- Option(superClass).orElse(moreParents.headOption)) {
            // TODO: handle more parents
            if (node.superClass == null) {
              node.superClass = firstParent
            }
          }

          if (node.typeParameters != null && typePars != null) {
            node.typeParameters.types ++= typePars.types
          } else if (typePars != null) {
            node.typeParameters = typePars.cloneNode()
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
                  MethodDefinition(_, _, _, funEx@AnyFun(astPars, body), _, _) = astMethod
                  methodId = symbols.ScopeContext.getNodeId(funEx)
                  t = Option(ret).orElse(retFun)
                  tt = t.flatMap(typeFromAST(_)(context))
                } {
                  val symId = SymbolMapId(funName, clsId)
                  types = types.handleParameterTypes(symId, tt, astPars, dtsPars, methodId)(context)
                }

              case MethodDefinition(Identifier(funName), Defined(ret), _, value, _, _) => // plain member with known type
                for (tt <- typeInfoFromAST(ret)(context)) {
                  types = types add SymbolMapId(funName, clsId) -> tt
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
        case FunctionDeclaration(Identifier(Id(name)), params, body, generator, ret) =>
          handleFunction(name, params, context.scopeId)
        case FunctionExpression(Identifier(Id(name)), params, body, generator, ret) =>
          handleFunction(name, params, context.scopeId)
        case c@ClassDeclaration(Identifier(name), superClass, moreParents, body, _) =>
          handleClass(c, name)
        case _: Node.Program =>
          false
        case _ =>
          true

      }
    }


    n.copy(top = newTop, types = types)
  }

  /*
  Transform enums represented in JS as objects:
  d.ts enum X {X0, X1} + js object X {X0: 0, X1: 1} => object X {val X0 = Value(0); val X1=Value(1)}
  */
  def transformEnumObjects(n: NodeExtended): NodeExtended = {
    val newBody = n.top.body.flatMap { node =>
      node match {
        // convert top level objects which represent known enums
        case Node.VariableDeclaration(Seq(vd@Node.VariableDeclarator(id@Identifier(name), oe: ObjectExpression, tpe)), kind) if enums.contains(name) =>
          val enumMembers = {
            oe.properties.map {
              case p@Property("init", _: Node.Identifier, false, pvalue@Literal(OrType(_: Double), _), false, _) =>
                val wrapValue = CallExpression(Identifier("Value"), Seq(pvalue)).withTokens(pvalue)
                val cloned = p.cloneNode()
                cloned.value = wrapValue
                cloned.readonly = true
                cloned
              case x =>
                x
            }
          }
          Seq(
            Node.VariableDeclaration(
              Seq(Node.VariableDeclarator(
                id, ObjectExpression(enumMembers).withTokens(oe), Node.TypeName(Seq(Node.Identifier("Enumeration").withTokens(oe)))
              ).copyLoc(vd)),
              kind
            ).copyLoc(node),
            TypeAliasDeclaration(id, TypeName(Seq(id, Identifier("Value").copyLoc(id)))).copyLoc(vd)
          )
        case _ =>
          Seq(node)
      }
    }

    val ret = n.top.cloneNode()
    ret.body = newBody
    n.copy(top = ret)
  }

  /*
  Transform enums represented in JS as values:
  d.ts enum X {}; const X0: X; const X1: X + js var X0 = 0; var X1 = 1 => object X {val X0 = Value(0); val X1=Value(1)};val X0 = X.X0;val X1 = X.X1
  */
  def transformEnumValues(n: NodeExtended): NodeExtended = {
    // gather all enum values declared in d.ts
    val enumGlobalValues = dtsSymbols.filter {
      case (_, VarDeclTyped(name, _, "const", Some(TypeName(Seq(Identifier(enumName)))))) if enums.contains(enumName) =>
        true
      case _ =>
        false
    }
    // gather all enum values defined in js
    val values = mutable.ArrayBuffer.empty[(String, VariableDeclaration)]
    n.top.walkWithScope {(node, ctx) =>
      implicit val c = ctx
      node match {
        case vd@VarDeclTyped(name, Some(value@Literal(OrType(_: Double), _)), kind, _) =>
          val id = Id(name)
          val tpe = n.types.get(Some(id))
          tpe.exists(_.declType match {
            case ClassType(t) if enums.contains(t.name) && enumGlobalValues.contains(name) =>
              values += t.name -> vd
              true
            case _ =>
              false
          })
        case x =>
          false
      }
    }
    // merge all definitions of the same enum into one
    val grouped = values.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
    val firstValues = grouped.view.mapValues(_.head).map(_.swap).toMap
    val allValues = values.map(_.swap).toMap

    val newBody = n.top.body.flatMap {
      case node@(vd: VariableDeclaration) =>
        // first value of each enum is transformed into an enum declaration
        val first = firstValues.get(vd).toSeq.flatMap { t =>
          val tEnumValues = grouped(t)
          val wrappedValues = tEnumValues.map {
            case vd@VarDeclTyped(name, Some(value@Literal(OrType(_: Double), _)), _, _) =>
              val wrapValue = CallExpression(Identifier("Value"), Seq(value)).withTokens(value)
              PropertyEx("init", Identifier(name).withTokens(vd), false, wrapValue, false, false, true).withTokens(vd)
          }
          Seq(
            VarDecl(
              t, Some(ObjectExpression(wrappedValues).withTokens(vd)), "const", Some(Node.TypeName(Seq(Identifier("Enumeration").withTokens(vd))))
            )(node),
            TypeAliasDeclaration(Identifier(t).withTokens(vd), TypeName(Seq(Identifier(t).withTokens(vd), Identifier("Value").withTokens(vd)))).withTokens(vd)
          )
        }

        // all values including the first are transformed into value aliases
        val dotValues = allValues.get(vd).map { t =>
          vd match {
            case vd@VarDeclTyped(name, _, _, _) =>
              VarDecl(
                name, Some(Dot(Identifier(t), Identifier(name)).withTokensDeep(vd)), "const", None
              )(node)
          }
        }

        if (first.nonEmpty || dotValues.nonEmpty) {
          first ++ dotValues
        } else {
          Seq(vd)
        }

      case node =>
        Seq(node)
    }
    val ret = n.top.cloneNode()
    ret.body = newBody
    n.copy(top = ret)
  }

  def transformEnums(n: NodeExtended): NodeExtended = {
    transformEnumValues(transformEnumObjects(n))
  }
}
