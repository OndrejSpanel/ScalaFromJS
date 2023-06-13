package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.ConvertProject._
import com.github.opengrabeso.scalafromjs.esprima._
import TypesRule._
import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.esprima.OrType
import com.github.opengrabeso.esprima.Node.{ArrayType => _, FunctionType => _, UnionType => _, _}
import SymbolTypes._
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId, symId}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try
import scala.collection.Seq
import scala.util.chaining.scalaUtilChainingOps

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

  def transformUnionEnums(n: NodeExtended): NodeExtended = {
    val rules = n.config.collectRules[TypesRule]
    rules.foldLeft(n)((c, r) => r.transformUnionEnums(c))
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
      case "object" => Some(ObjectOrMap)
      case "any" => None // any is usually a placeholder for missing information
      case "never" => Some(NothingType)
      case "void" => Some(NoType)
      case "this" => None // TODO: some better support for this type
      case "Array" => Some(ArrayType(AnyType))
      case _ => Some(ClassType(context.findSymId(name, memberFun)))
    }
    t
  }

  object IsDouble {
    def unapply(x: String): Option[Double] = Try(x.toDouble).toOption
  }
  object IsInt {
    def unapply(x: String): Option[Int] = Try(x.toInt).toOption
  }

  def typeFromLiteral(raw: String): Option[TypeDesc] = raw match {
    case "true" => Some(LiteralTypeDesc(true))
    case "false" => Some(LiteralTypeDesc(false))
    case IsInt(x) => Some(LiteralTypeDesc(x))
    case IsDouble(x) => Some(LiteralTypeDesc(x))
    case s if s.head == '"' && s.last == '"' || s.head == '\'' && s.last == '\'' => Some(LiteralTypeDesc(s.drop(1).dropRight(1)))
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
    typeFromAST(tpe)(context).map(TypeInfo.certain)
  }
}
case class TypesRule(types: String, root: String, fs: FileEnvironment) extends ExternalRule {
  val project = ConvertProject.loadControlFile(PathUtils.resolveSibling(root, types), fs)
  // load the d.ts
  val dtsSymbols = loadSymbols(project.code)
  val dtsRange = (0, project.offsets.last)
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

    // scan objects (variables) or native enums (created by transformEnumValues)  present in the JS
    val objects = {
      val objectBuilder = Set.newBuilder[String]
      n.top.walkWithScope { (node, ctx) =>
        node match {
          case Node.VariableDeclarator(Identifier(name), oe: ObjectExpression, _) =>
            objectBuilder += name
            false
          case Node.EnumDeclaration(Identifier(name), _ ) =>
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
          /* // do not include enums, they will be added by transformEnumValues
          case e@Node.EnumDeclaration(Identifier(name), body) if !objects.contains(name) && body.body.isEmpty =>
            e
           */
          case e@Node.TypeAliasDeclaration(Identifier(name), _) if !objects.contains(name) =>
            e
        }
      } else {
        item.fullName -> Nil
      }
    }.toMap


    // scope offsets need to be unique. For identification of symbols coming from D.TS only we pretend D.TS files are appended after all JS files
    // we do not have access to JS project here. assume there will always be less than 1 GB of JS code
    //val dtsOffset = 1_000_000_000 // assume there will always be less than 1 GB of JS code
    val dtsOffset = 1_000_000_000 // assume there will always be less than 1 GB of JS code
    val beginMarker = ConvertProject.prefixName + "begin"

    val dtsTopLevelRange = n.top.range
    // scan the top-level body for the markers, and insert the new content as needed
    // do this before adding types for the symbols, as that should handle the newly added ones as well
    // markers may have unique postfix - see declaredGlobal
    val newBody = n.top.body.flatMap {
      case node@Node.ExportNamedDeclaration(VarDecl(name, Some(Node.Literal(value, raw)), "const"), Seq(), null) if name.startsWith(beginMarker) =>
        // we want to insert declarations somewhere after the "begin" markers
        // TODO: insert after imports (if there are any)
        val dtsFromJS = value.replaceAll(".js$", ".d.ts")
        symbolsToInclude.get(dtsFromJS) match {
          case Some(injectSymbols) =>
            val symbolsInJS = injectSymbols.toSeq.map(_.cloneDeep())
            def adjustRange(node: Node): Unit = {
              if (node.range != null) {
                assert(node.range._1 < dtsOffset)
                node.range = (node.range._1 + dtsOffset, node.range._2 + dtsOffset)
              }
            }
            symbolsInJS.foreach { symNode =>
              symNode.walk { n =>
                adjustRange(n)
                false
              }
            }
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
                    types = types add context.findSymId(name, false) -> TypeInfo.certain(enumType)
                    true
                  case _ =>
                    false
                }
              case _ =>
                false
            } || enumValues.get(name).exists { e =>
              // TODO: proper scoped type (source global, not JS global)
              val enumType = ClassType(SymbolMapId(e.name.name, (-1, -1)))
              types = types add context.findSymId(name, false) -> TypeInfo.certain(enumType)
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
              case p@Property("init", pid: Node.Identifier, false, pvalue@Literal(OrType(_: Double), _), false, _) =>
                val wrapValue = CallExpression(Identifier("Value").withTokens(pvalue), Seq(pvalue)).withTokens(pvalue)
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
              ).withTokens(vd)),
              kind
            ).copyLoc(node),
            TypeAliasDeclaration(id, TypeName(Seq(id, Identifier("Value").withTokens(id))).withTokens(id)).withTokens(vd)
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
  Transform enums represented in JS as variables to TypeScript enums with type aliases
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
              EnumBodyElement(Identifier(name).withTokens(value), value).withTokens(value)
          }
          Seq(
            EnumDeclaration(Identifier(t), EnumBody(wrappedValues)).withTokensDeep(vd),
            TypeAliasDeclaration(Identifier(t), TypeName(Seq(Identifier(t), Identifier("Value")))).withTokensDeep(vd)
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

  /** transform enums defined as `const A: 0; const B: 1; type E = typeof A | typeof B` */
  def transformUnionEnums(n: NodeExtended): NodeExtended = {

    object NormalizedUnionType {

      private def normalize(t: Node.UnionType): Seq[Node.TypeAnnotation] = {
        val left = t.left match {
          case x: Node.UnionType => normalize(x)
          case x => List(x)
        }
        val right = t.right match {
          case x: Node.UnionType => normalize(x)
          case x => List(x)
        }
        left ++ right
      }

      def unapplySeq(t: Node.UnionType): Some[Seq[Node.TypeAnnotation]] = {
        Some(normalize(t))
      }
    }

    // gather all possible enum values (all variables with a literal type)
    val possibleEnumValues = dtsSymbols.flatMap {
      case (name, VarDeclTyped(xName, _, _, Some(Node.LiteralType(tpe)))) =>
        assert(name == xName)
        Some(name -> tpe)
      case _ =>
        None

    }

    val enumTypes = dtsSymbols.flatMap {
      case (_, TypeAliasDeclaration(Identifier(name), NormalizedUnionType(types@_*))) =>
        val typeNames = types.map {
          case t: Node.TypeName if t.parent.size == 1 && possibleEnumValues.contains(t.parent.head.name) =>
            // check if the type name references a literal type
            Some(t.parent.head.name)
          case _ =>
            None
        }
        if (typeNames.forall(_.isDefined)) {
          Some(name -> typeNames.flatten)
        } else None

      case _ =>
        None
    }
    val enumTypesFromNames = enumTypes.flatMap { case (name, types) =>
      types.map(_ -> name)
    }

    // gather all enum values defined in js
    val values = mutable.ArrayBuffer.empty[(String, VariableDeclaration, SymId)]
    n.top.walkWithScope { (node, ctx) =>
      implicit val c = ctx
      node match {
        case vd@VarDeclTyped(name, Some(value: Literal), _, _) if possibleEnumValues.get(name).contains(value) =>
          enumTypesFromNames.get(name) match {
            case Some(enumType) =>
              values += ((enumType, vd, symId(name).get))
              true
            case None =>
              false
          }
        case _ =>
          false
      }
    }

    // merge all definitions of the same enum into one
    val grouped = values.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
    val firstValues = grouped.view.mapValues(_.head).map(_.swap).toMap
    val allValues = values.map(x => (x._2, (x._1, x._3))).toMap

    val newBody = n.top.body.flatMap {
      case tpe: TypeAliasDeclaration if enumTypes.contains(tpe.id.name) =>
        Nil
      case node@(vd: VariableDeclaration) =>
        // first value of each enum is transformed into an enum declaration
        val first = firstValues.get(vd).toSeq.flatMap { t =>
          val tEnumValues = grouped(t)
          val wrappedValues = tEnumValues.map {
            case VarDeclTyped(name, Some(value: Literal), _, _) =>
              EnumBodyElement(Identifier(name).withTokens(value), value).withTokens(value)
          }
          Seq(
            EnumDeclaration(Identifier(t), EnumBody(wrappedValues)).withTokensDeep(vd),
            TypeAliasDeclaration(Identifier(t), TypeName(Seq(Identifier(t), Identifier("Value")))).withTokensDeep(vd)
          )
        }

        // all values including the first are transformed into value aliases
        val dotValues = allValues.get(vd).map { t =>
          vd match {
            case vd@VarDeclTyped(name, _, _, _) =>
              VarDecl(
                name, Some(Dot(Identifier(t._1), Identifier(name)).withTokensDeep(vd)), "const", Some(TypeName(Seq(Identifier(t._1))).withTokensDeep(vd))
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

    // we assume all enums are top-level
    val globalScopeId = ScopeContext.getNodeId(n.top)

    val overrideEnumValueTypes = values.foldLeft(n.types) { case (types, (enumName, decl, id)) =>
      types.add(id -> TypeInfo.certain(ClassType(SymId(enumName, globalScopeId))))
    }
    val overrideEnumTypes = enumTypes.foldLeft(overrideEnumValueTypes) { case (types, (enumType, enumValues)) =>
      val enumSymId = SymId(enumType, globalScopeId)
      types
        .add(SymId(enumType, globalScopeId) -> TypeInfo.certain(ClassType(enumSymId)))
        .remove(SymId.global(enumType)) // hotfix: enum type aliases are introduced in the global scope (no corresponding JS source node)

    }
    n.copy(top = ret, types = overrideEnumTypes)
  }

  def transformEnums(n: NodeExtended): NodeExtended = {
    transformEnumValues(transformEnumObjects(n))
  }
}
