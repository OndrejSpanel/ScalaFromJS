package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import Expressions._
import ScalaOut.SymbolDef
import com.github.opengrabeso.scalafromjs
import com.github.opengrabeso.scalafromjs.esprima.symbols._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.collection.Seq

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  import SymbolTypes._
  import Symbols._

  def symbolType(types: SymbolTypes, symbol: Node.Identifier)(implicit context: ScopeContext): Option[TypeInfo] = {
    types.get(symbolId(symbol))
  }

  def symbolId(symbol: Node.Identifier)(implicit context: ScopeContext): Option[SymbolMapId] = {
    symId(symbol)
  }

  def identifierFromPar(p: Node.Node): Option[(Node.Identifier, Node.TypeAnnotation, Option[Node.Expression])] = p match {
    case x: Node.Identifier =>
      Some(x, null, None)
    case Node.FunctionParameterWithType(x: Node.Identifier, tpe, defValue, _) =>
      Some(x, tpe, Option(defValue))
    case Node.AssignmentPattern(x: Node.Identifier, init) =>
      Some(x, null, Some(init))
    case Node.RestElement(x: Node.Identifier, tpe) =>
      Some(x, tpe, None)
    case _ =>
      None
  }

  def funArg(p: Node.FunctionParameter): (Node.Identifier, Node.TypeAnnotation, Option[Node.Expression]) = identifierFromPar(p).get
  def nameFromPar(p: Node.FunctionParameter): Option[String] = identifierFromPar(p).map(_._1.name)
  def typeFromPar(p: Node.FunctionParameter): Option[Node.TypeAnnotation] = identifierFromPar(p).map(_._2)

  object ParName {
    def unapply(p: Node.FunctionParameter) = nameFromPar(p)
  }

  def symbolFromPar(p: Node.FunctionParameter)(implicit context: ScopeContext): Option[SymId] = {
    val s = nameFromPar(p)
    s.flatMap(symId)
  }

  // individual sensible transformations

  // convert === to ==
  def relations(n: Node.Node): Node.Node = {
    n.transformAfter { (node, _) =>
      node match {
        case bin@Binary(_, "===", _) =>
          bin.operator = "=="
          node
        case bin@Binary(_, "!==", _) =>
          bin.operator = "!="
          node
        case _ =>
          node
      }
    }
  }



  def handleIncrement(n: Node.Node): Node.Node = {

    def substitute(node: Node.Node, expr: Node.Expression, op: String) = {
      new Node.AssignmentExpression(
        operator = op match {
          case "++" => "+="
          case "--" => "-="
        },
        left = expr,
        right = new Node.Literal(1.0, "1").copyLoc(node)
      ).copyLoc(node)
    }

    // walk the tree, check for increment / decrement
    n.transformAfter { (node, transformer) =>
      def nodeResultDiscarded(n: Node.Node, parentLevel: Int): Boolean = {
        transformer.parent(parentLevel) match {
          case Some(_: Node.ExpressionStatement) =>
            true
          case Some(f: Node.ForStatement) =>
            // can be substituted inside of for unless used as a condition
            Option(f.init).contains(n) || Option(f.update).contains(n)
          case Some(s: Node.SequenceExpression) =>
            if (s.expressions.last !=n) true
            else if (parentLevel < transformer.parentCount -1) {
              // even last item of seq can be substituted when the seq result is discarded
              nodeResultDiscarded(s, parentLevel+1)
            } else false
          case None =>
            true
          case _ =>
            false
        }
      }

      node match {
        case Node.UpdateExpression(op, expr, prefix) =>
          if (nodeResultDiscarded(node, 0)) {
            substitute(node, expr, op)
          } else {
            val operation = Node.ExpressionStatement(substitute(node, expr, op)).copyLoc(node)
            ScalaNode.StatementExpression {
              Node.BlockStatement {
                if (prefix) {
                  val value = Node.ExpressionStatement(expr.cloneNode()).copyLoc(expr)
                  Seq(operation, value)
                } else {
                  val tempName = "temp"
                  val storeValue = Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(tempName), expr.cloneNode(), null)), "const")
                  val loadValue = Node.ExpressionStatement(Node.Identifier(tempName)).copyLoc(expr)
                  Seq(storeValue, operation, loadValue)
                }
              }
            }.withTokensDeep(expr)
          }
        case _ =>
          node
      }
    }
  }

  def replaceReturnWithStatement(ret: Node.ReturnStatement) = {
    Option(ret.argument).fold[Node.Statement] {
      Node.EmptyStatement().copyLoc(ret)
    } {
      Node.ExpressionStatement(_).copyLoc(ret)
    }
  }

  // TODO: handle scope context while walking
  def walkLastNode(n: Node.Node)(callback: Node.Node => Boolean): Boolean = {
    n match {
      case s: Node.BlockStatement =>
        s.body.lastOption.fold(false) { l =>
          val r = callback(l)
          if (!r) walkLastNode(l)(callback)
          r
        }
      case s: Node.ExpressionStatement =>
        val r = callback(s.expression)
        if (!r) walkLastNode(s.expression)(callback)
        r
      case _ =>
        false

    }
  }

  // is this a last node in some scope? (walks also parents)
  def nodeLast(n: Node.Node, parentLevel: Int, transformer: TreeTransformer): Boolean = {
    //println(s"nodeLast ${nodeClassName(n)}:$parentLevel:${transformer.parent(parentLevel).map(nodeClassName)}")
    transformer.parent(parentLevel) match {
      case Some(ss: Node.ExpressionStatement) =>
        ss.expression == n
      case Some(ss: ScalaNode.StatementExpression) =>
        ss.statement == n
      case Some(fun: Node.FunctionDeclaration) =>
        fun.body == n
      case Some(fun: Node.FunctionExpression) =>
        fun.body == n
      case Some(block: Node.BlockStatement) =>
        (block.body.lastOption contains n) && parentLevel < transformer.parentCount - 1 && nodeLast(block, parentLevel + 1, transformer)
      case Some(ii: Node.IfStatement) =>
        (ii.consequent == n || Option(ii.alternate).contains(n)) && nodeLast(ii, parentLevel + 1, transformer)
      case None =>
        true
      case _ =>
        false
    }
  }

  def removeTrailingReturn(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>

      node match {
        case ret: Node.ReturnStatement if nodeLast(ret, 0, transformer) =>
          // check if last in a function body
          //println("Remove trailing return")
          replaceReturnWithStatement(ret)
        case _ =>
          node
      }
    }
  }

  def removeReturnFromBody(body: Seq[Node.StatementListItem]): Seq[Node.StatementListItem] = {
    // remove all direct returns
    body.dropRight(1) ++ body.lastOption.map {
      _.transformBefore { (node, descend, transformer) =>
        node match {
          case _: Node.FunctionExpression =>
            // do not descend into any other functions
            node
          case ret: Node.ReturnStatement if nodeLast(ret, 0, transformer) =>
            //println(s"Remove return of ${nodeTreeToString(ret)}")
            replaceReturnWithStatement(ret)
          case ret: Node.ReturnStatement =>
            //println(s"No remove return of ${nodeTreeToString(ret)}")
            node
          case _ =>
            descend(node, transformer)
            node
        }
      }
    }
  }

  def removeTrailingBreak(n: Node.Node): Node.Node = {

    n.transformAfter { (node, transformer) =>

      node match {
        case sw: Node.SwitchStatement =>
          // group conditions with empty body with the following non-empty one
          def conditionGroups(s: Seq[Node.SwitchCase], ret: Seq[Seq[Node.SwitchCase]]): Seq[Seq[Node.SwitchCase]] = s match {
            case Seq() =>
              ret
            case seq =>
              val (empty, nonEmpty) = seq.span (_.consequent.isEmpty)
              conditionGroups(nonEmpty.drop(1), ret :+ (empty ++ nonEmpty.headOption))
          }

          val body = sw.cases
          val conditionGrouped = conditionGroups(body, Seq())

          val groupedBody = for (g <- conditionGrouped) yield {
            // all but the last are empty
            def processGroup(e: Seq[Node.SwitchCase], ret: Node.SwitchCase): Node.SwitchCase = {
              def join(c1: Node.SwitchCase, c2: Node.SwitchCase) = {
                //println(s"Join ${ScalaOut.outputNode(c1)} ${ScalaOut.outputNode(c2)}")
                assert(c1.consequent.isEmpty)
                (c1, c2) match {
                  case (Node.SwitchCase(Defined(test1), body1), Node.SwitchCase(Defined(test2), body2)) =>
                    c2.test = Node.BinaryExpression("|", test1, test2).withTokens(c2)
                    c2
                  case (Node.SwitchCase(null, body1), Node.SwitchCase(Defined(_), body2)) =>
                    c1.consequent = body2
                    c1
                  case (_, case2) =>
                    assert(case2.test == null)
                    case2
                }

              }
              e match {
                case head +: tail =>
                  processGroup(tail, join(ret, head))

                case _ =>
                  ret
              }
            }
            processGroup(g.tail, g.head)
          }

          val newBody = new mutable.ArrayBuffer[Node.SwitchCase]
          for (s <- groupedBody) {
            s.consequent.lastOption match {
              case Some(_: Node.BreakStatement) =>
                s.consequent = s.consequent.dropRight(1)
                newBody append s
              case Some(_: Node.ThrowStatement) =>
                newBody append s
              case Some(_: Node.ReturnStatement) =>
                newBody append s
              case _  =>
                // fall through branches - warn
                if (s != groupedBody.last) {
                  s.consequent = s.consequent :+ unsupported("Missing break", s)
                }
                newBody append s
            }
          }
          sw.cases = newBody
          sw
        case _ =>
          node
      }
    }
  }

  def readJSDoc(n: NodeExtended): NodeExtended = {
    var commentsParsed = Set.empty[Int]

    val declBuffer = new mutable.ArrayBuffer[(SymId, TypeDesc)]()

    n.top.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case f: DefFun =>
          for {
            commentToken <- Option(f.leadingComments).flatMap(_.lastOption)
            pos <- f.start
          } {
            val comment = commentToken.value
            if (!(commentsParsed contains pos)) {
              commentsParsed += pos
              for (commentLine <- comment.linesIterator) {
                // match anything in form:

                val JSDocParam = """@param +([^ ]+) +\{([^ ]+)\}""".r.unanchored // @param name {type}
                val JSDocReturn = """@return +\{([^ ]+)\}""".r.unanchored // @return {type}
                commentLine match {
                  case JSDocParam(name, tpe) =>
                    // verify the parameter exists
                    if (f.params.exists(parameterNameString(_) == name)) {
                      val td = Id(name)
                      parseType(tpe).foreach(t => declBuffer append td -> t)
                    }
                  case JSDocReturn(tpe) =>
                    val td = Id(f.id)
                    parseType(tpe).foreach(t => declBuffer append td -> t)
                  case _ =>
                }
              }
            }
          }
        case _ =>
      }
      false
    }
    n.copy(types = n.types ++ SymbolTypes(declBuffer))
  }

  class ExtractorInList(ops: String*) {
    def unapply(arg: String): Boolean =  ops.contains(arg)
  }

  object IsArithmetic extends ExtractorInList("-", "*", "/", "%", "^", "&", "<<", ">>")

  object IsComparison extends ExtractorInList("==", "!=", "<=", ">=", ">", "<", "===", "!==")

  object IsBoolean extends ExtractorInList("||", "&&")

  case class Ref[T](var t: T)

  implicit def refToT[T](r: Ref[T]): T = r.t

  // TODO: no need to pass both classInfo and classes, one of them should do
  case class ExpressionTypeContext(types: Ref[SymbolTypes], classInfo: ClassInfo, classes: ClassListHarmony) {
    implicit object classOps extends ClassOps {
      def mostDerived(c1: ClassType, c2: ClassType) = {
        //println("mostDerived")
        classInfo.mostDerived(c1.name, c2.name).fold[TypeDesc](any)(ClassType.apply)
      }

      def commonBase(c1: ClassType, c2: ClassType) = {
        //println("commonBase")
        classInfo.commonBase(c1.name, c2.name).fold[TypeDesc](any)(ClassType.apply)
      }

      def resolveClass(c: ClassType) = {
        // if the type alias is present, use it, otherwise pass unchanged
        types.t.types.get(c.name).map(_.declType).getOrElse(c)
      }
    }

    implicit val classPos: (SymbolMapId) => (Int, Int) = classes.classPos

  }

  object TypeDecl {
    def unapply(arg: TypeInfo) = Some(arg.declType)
  }

  def callReturn(funType: TypeInfo): TypeInfo = funType.declType match {
    case FunctionType(ret, _) =>
      //println(s"callReturn $ret")
      TypeInfo.target(ret)
    case _ =>
      //println(s"callReturn funType $funType")
      funType
  }

  object ExpressionType {
    def unapply(arg: Node.Node)(implicit ctx: ExpressionTypeContext, context: ScopeContext): Option[Option[TypeInfo]] = {
      Some(expressionType(arg))
    }
  }

  object CanBeArray {
    def unapply(tpe: TypeDesc): Option[ArrayType] = {
      tpe match {
        case a: ArrayType => Some(a)
        case u: UnionType => u.types.collectFirst {
          case a: ArrayType =>
            a
        }
        case _ =>
          None
      }
    }
  }
  object CanBeMap {
    def unapply(tpe: TypeDesc): Option[MapType] = {
      tpe match {
        case a: MapType => Some(a)
        case u: UnionType => u.types.collectFirst {
          case a: MapType =>
            a
        }
        case _ =>
          None
      }
    }
  }

  def expressionType(n: Node.Node, log: Boolean = false)(implicit ctx: ExpressionTypeContext, context: ScopeContext): Option[TypeInfo] = {
    import ctx._
    //println(s"  type ${nodeClassName(n)}: ${ScalaOut.outputNode(n)}")

    /*
    * @param certain when context indicates it is a class name (in new or instanceof)
    * */
    def typeInfoFromClassSym(classSym: SymbolDef, certain: Boolean = false): Option[TypeInfo] = {
      // is the symbol a class?
      classSym.filter(_.name == "<anonymous>").map { c =>
        TypeInfo.target(AnonymousClassType(c.sourcePos))
      }.orElse {
        classSym.filter(_ == SymId("Array", (-1, -1))).map(_ => TypeInfo.target(ArrayType(AnyType)))
      }.orElse {
        for {
          cls <- classSym
          if certain || ctx.classes.get(cls).isDefined
        } yield {
          TypeInfo.target(ClassType(cls))
        }
      }
    }

    def typeInfoFromClassDef(classDef: Option[Node.ClassDeclaration]) = {
      classDef.map(_.id).map(Id.apply).flatMap(typeInfoFromClassSym(_))
    }

    n match {
      case s: Node.Super =>
        val sup = findSuperClass(context)
        //println(s"super scope $sup")
        sup.map(t => TypeInfo.target(ClassType(t)))

      case t: Node.ThisExpression =>
        val thisScope = findThisClass(context) // TODO: consider this in a function
        //println(s"this scope ${t.scope.map(_.nesting)}")
        typeInfoFromClassDef(thisScope)
        //println(s"this def scope $cls")

      case Node.Identifier("undefined")  =>
        // not allowing undefined overrides
        None // Some(TypeInfo(AnyType, NoType))

      case Node.Identifier(Id(symDef)) =>
        // if the symbol is a class name, use it as a class type directly
        val rt = types.get(symDef).orElse(typeInfoFromClassSym(symDef))
        rt

        //println(s"    Sym ${symDef.name} type $rt")

      case expr Dot name =>
        val exprType = expressionType(expr, log)(ctx, context)
        if (log) println(s"type of member $expr.$name, class $exprType")
        exprType match {
          case Some(TypeDecl(ClassType(callOn))) =>
            for {
              c <- findInParents(callOn, name)(ctx)
              r <- types.getMember(Some(MemberId(c, name)))
            } yield {
              if (log) println(s"type of member $c.$name as $r")
              r
            }
          case Some(TypeDecl(AnonymousClassType(sourcePos))) =>
            val id = SymbolMapId(name, sourcePos)
            types.get(Some(id))
          case _ =>
            None

        }

      case Node.ObjectExpression(Nil) =>
        // avoid handling empty object as an anonymous class - handling it as a map has more sense
        Some(TypeInfo.target(ObjectOrMap))

      case oe: Node.ObjectExpression =>
        val nodeId = ScopeContext.getNodeId(oe)
        typeInfoFromClassSym(Some(SymbolMapId("<anonymous>", nodeId)))

      case expr Sub name =>
        //println(s"Infer type of array item $name, et ${expressionType(expr)(ctx)}")
        expressionType(expr, log) match {
          case Some(TypeDecl(CanBeArray(ArrayType(item)))) =>
            val r = TypeInfo.target(item)
            if (log) println(s"type of array $expr.$name as $r")
            Some(r)
          case Some(TypeDecl(CanBeMap(MapType(item)))) =>
            val r = Some(TypeInfo.target(item))
            if (log) println(s"type of map $expr.$name as $r")
            r
          case _ =>
            None
        }

      case a: AArray =>
        val elementTypes = a.elements.map(expressionType(_, log))
        val elType = elementTypes.reduceOption(typeUnionOption).flatten
        if (log) {
          println(s"  elementTypes $elementTypes => $elType")
        }
        Some(elType.map(_.map(ArrayType)).getOrElse(TypeInfo(ArrayType(AnyType), ArrayType(NoType))))
      case Node.Literal(Defined(literal), _) =>
        literal.value match {
          case _: Double =>
            Some(TypeInfo.target(number))
          case _: String =>
            Some(TypeInfo.target(string))
          case _: Boolean =>
            Some(TypeInfo.target(boolean))
          case _ =>
            None
        }

      case Node.CallExpression(expr Dot "join", Seq()) if (expressionType(expr).exists(_.declType.isInstanceOf[ArrayType])) =>
        Some(TypeInfo.target(SimpleType("String")))

      // Array.isArray( name ) ? name : [name]
      case Node.ConditionalExpression(Node.CallExpression(Node.Identifier("Array") Dot "isArray",  Seq(isArrayArg)), exprTrue, exprFalse) =>
        val exprType = expressionType(isArrayArg, log)
        //println(s"ExprType $exprType of Array.isArray( name ) ? name : [name] for $n1")
        if(exprType.exists(_.declType.isInstanceOf[ArrayType])) {
          expressionType(exprTrue, log)
        } else {
          expressionType(exprFalse, log)
        }
      case Node.ConditionalExpression(_, te, fe) =>
        val t = expressionType(te, log)
        val f = expressionType(fe, log)
        val ret = typeUnionOption(t, f)
        if (log) println(s"Node.Conditional $te:$t / $fe:$f = $ret")
        ret

      case Binary(expr, "as", Node.Identifier(cls)) => // typescript operator
        transform.TypesRule.typeFromIdentifierName(cls)(context).map(TypeInfo.both)

      case Binary(expr, `asinstanceof`, Node.Identifier(cls)) =>
        transform.TypesRule.typeFromIdentifierName(cls)(context).map(TypeInfo.both)

      case Binary(left, op, right) =>
        // sometimes operation is enough to guess an expression type
        // result of any arithmetic op is a number
        op match {
          case IsArithmetic() => Some(TypeInfo.target(number))
          case IsComparison() => Some(TypeInfo.target(boolean))
          case "+" =>
            val typeLeft = expressionType(left, log)
            val typeRight = expressionType(right, log)
            // string + anything is a string
            if (typeLeft == typeRight) typeLeft
            else if (typeLeft.exists(_.target == string) || typeRight.exists(_.target == string)) Some(TypeInfo.target(string))
            else None
          case "||" =>
            // boolean with the same type is the same type
            val typeLeft = expressionType(left, log)
            val typeRight = expressionType(right, log)
            // prefer left type, like in true || xxx
            typeLeft.orElse(typeRight)

          case "&&" =>
            // boolean with the same type is the same type
            val typeLeft = expressionType(left, log)
            val typeRight = expressionType(right, log)
            // prefer right type, like in true && xxx
            typeRight.orElse(typeLeft)


          case _ =>
            None
        }
      case Node.NewExpression(Node.Identifier(Id(call)), _) =>
        typeInfoFromClassSym(call, true)

      case Node.NewExpression(Node.Identifier(name1) Dot "array" Dot "constructor", _) =>
        Some(TypeInfo(ArrayType(AnyType), ArrayType(NoType)))

      case Node.NewExpression(Node.Identifier("array") Dot "constructor", _) =>
        Some(TypeInfo(ArrayType(AnyType), ArrayType(NoType)))

      case Node.NewExpression(expr Dot "constructor", _) =>
        expressionType(expr)

      case Node.NewExpression(Node.Identifier(name) Dot call, _) =>
        if (call != "constructor") {
          Some(TypeInfo.both(ClassType(SymbolMapId(call, 0 -> 0))))
        } else {
          None
        }

      case Node.CallExpression(Node.Identifier(Id(call)), _) =>
        val tid = id(call)
        if (log)  println(s"Infer type of call ${call.name}:$tid as ${types.get(tid)}")
        types.get(tid).map(callReturn)

      case Node.CallExpression(cls Dot name, _) =>
        val exprType = expressionType(cls, log)
        if (log) println(s"Infer type of member call $cls.$name class $exprType")

        {
          for {
            TypeDecl(ClassType(callOn)) <- exprType
            c <- findInParents(callOn, name)(ctx)
            r <- types.getMember(Some(MemberId(c, name)))
          } yield {
            if (log) println(s"  Infer type of member call $c.$name as $r")
            callReturn(r)
          }
        }.orElse {
          for {
            TypeDecl(AnonymousClassType(callOn)) <- exprType
            r <- types.get(Some(SymbolMapId(name, callOn)))
          } yield {
            if (log) println(s"  Infer type of anonymous member call $name as $r")
            callReturn(r)
          }
        }

      case seq: Node.SequenceExpression =>
        expressionType(seq.expressions.last, log)
      case Node.ExpressionStatement(ExpressionType(t)) =>
        t

      case Node.BlockStatement( skipped :+ last) =>
        context.withScope(n) {
          // TODO: handle in ScopeContext.enterScope instead
          skipped.foreach(context.scanSymbols)
          expressionType(last)
        }
      case ScalaNode.StatementExpression(expression) =>
        expressionType(expression)
      case Node.ExpressionStatement(expression) =>
        expressionType(expression)
      case fun@AnyFun(args, body) =>
        val returnType = transform.InferTypes.scanFunctionReturns(Block(body).withTokens(fun))
        // TODO: use inferred argument types as well
        val argTypes = args.map(_ => any).toIndexedSeq
        if (log) println(s"lambda returns $returnType")
        returnType.map { rt =>
          TypeInfo.target(FunctionType(rt.declType, argTypes))
        }
      case _ =>
        None

    }
  }

  def isReadOnlyProperty(cls: Node.ClassDeclaration, name: String): Option[Node.Expression] = {
    var getter = Option.empty[Node.Expression]
    var setter = false
    cls.body.body.filter(p => methodName(p) == name).foreach {
      case Node.MethodDefinition(Node.Identifier(p), null, _, AnyFun(Seq(), value), "get", _) =>
        value match {
          case ex: Node.Expression =>
            getter = Some(ex)
          case Node.BlockStatement(Seq(Node.ExpressionStatement(ex))) =>
            getter = Some(ex)
          case bs: Node.BlockStatement =>
            val wrapped = ScalaNode.StatementExpression(bs).withTokens(bs)
            getter = Some(wrapped)
        }
      case Node.MethodDefinition(Node.Identifier(p),  _, _, _, "set", _) =>
        setter = true
      case Node.MethodDefinition(Node.Identifier(p), _, _, _, _, _) =>
        //println(s"  function $p")
        //getter = true
      case ObjectKeyVal(p, value) =>
        //println(s"  keyval $p")
        value match {
          case ex: Node.Expression =>
            getter = Some(ex)
          case _ =>
            ???
        }
      case _ =>
    }
    if (!setter) getter
    else None
  }

  def listPrototypeMemberNames(cls: Node.ClassDeclaration): Seq[String] = {
    var existingMembers = Seq.empty[String]

    /*
    def addAccessor(s: Node.ObjectSetterOrGetter) = {
      s.key match {
        case Node.Identifier(name) =>
          existingMembers = existingMembers :+ name
        case _ =>
      }
    }
    */

    Option(cls.body).foreach(_.body.foreach {
      case md: Node.MethodDefinition if md.key != null =>
        existingMembers = existingMembers :+ propertyKeyName(md.key)
      //case ObjectKeyVal(p, _) =>
      //  existingMembers = existingMembers :+ p
      /*
      case s: Node.ObjectSetter =>
        addAccessor(s)
      case s: Node.ObjectGetter =>
        addAccessor(s)
      */
      case _ =>
    })
    existingMembers
  }

  def listDefinedClassMembers(ast: NodeExtended) = {
    var listMembers = ClassInfo()
    ast.top.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case cls@Node.ClassDeclaration(Node.Identifier(Id(clsSym)), parentNode, moreParents, _, _) =>
          for (clsId <- id(clsSym)) {
            for (Node.Identifier(Id(parentId)) <- Option(parentNode)) {
              //println(s"Add parent $parent for $clsSym")
              listMembers = listMembers.copy(parents = listMembers.parents + (clsId -> parentId))
            }
            val members = listPrototypeMemberNames(cls)

            // list data members
            //println(s"varMembers for $cls")
            val varMembers = for {
              inlineMethod <- findInlineBody(cls).toSeq
              inlineBodyBlock <- getMethodBody(inlineMethod).toSeq
              VarDecl(member, _, _) <- inlineBodyBlock.body
            } yield member

            // TODO: maybe parMembersInline is enough?
            def listParameters(method: Option[Node.MethodDefinition]): Seq[SymId] = method.toSeq.flatMap {
              _.value match {
                case AnyFun(params, body) =>
                  params.flatMap(Transform.symbolFromPar(_))
              }
            }

            val parMembers = listParameters(findConstructor(cls)).map(_.name)
            val parMembersInline = for {
              arg <- listParameters(findInlineBody(cls))
              argId = clsId.copy(name = arg.name) // arg may be constructed by us - may miss symbol/token information
              //_ = println(s"Arg $argId from cls $clsId hints ${node.types.hints.get(argId)}")
              if !ast.types.hints.get(argId).contains(IsConstructorParameter) && !arg.name.endsWith(parSuffix)
            } yield {
              arg.name
            }

            //println(s"${clsSym.name}: parMembers $parMembers $parMembersInline")
            val clsMembers = clsId -> (members ++ varMembers ++ parMembers ++ parMembersInline).distinct
            listMembers = listMembers.copy(members = listMembers.members + clsMembers)

            //println(s"listMembers $listMembers (++ $clsMembers)")
          }
          false
        case _ =>
          false
      }
    }
    listMembers
  }

  def listClassMembers(node: NodeExtended) = {
    SymbolTypes.stdClassInfo ++ listDefinedClassMembers(node)
  }

  // get rid of transform var XXX = function(){ function XXX(); return XXX; }()
  def removeVarClassScope(n: Node.Node): Node.Node = {
    object DefineAndReturnClass {
      private object ReturnedExpression {
        def unapply(arg: Node.Node) = arg match {
          case Node.ReturnStatement(Defined(x)) => // regular return
            Some(x)
          case Node.ExpressionStatement(x) => // elided trailing return
            Some(x)
          case _ =>
            None
        }
      }

      def unapply(arg: Seq[Node.Node])(implicit context: ScopeContext): Option[(Node.Node, SymId)] = {
        val compact = arg.filterNot(_.isInstanceOf[Node.EmptyStatement])
        compact match {
          case Seq(defClass@Node.ClassDeclaration(Defined(Id(c)), _, _, _, _), ReturnedExpression(Node.Identifier(Id(r)))) if c == r =>
            Some(defClass, c)
          case _ =>
            None
        }
      }
    }

    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDecl(sym, Some(ScalaNode.StatementExpression(Node.BlockStatement(DefineAndReturnClass(defClass, r)))), _) if sym == r.name =>
          defClass
        case _ =>
          node
      }
    }
  }

  def iife(n: Node.Node): Node.Node = {
    //println(nodeTreeToString(n.top))

    // "Immediately-invoked function expression"
    object IIFE {
      def unapply(arg: Node.Node) = arg match {
        case Node.CallExpression(AnyFun(args1, funcBody), args2) if args1.isEmpty && args2.isEmpty =>
          Some(funcBody)
        case _ => None

      }
    }

    n.transformAfter { (node, _) =>
      node match {
        case IIFE(funcBody) =>
          ScalaNode.StatementExpression{
            Node.BlockStatement {
              removeReturnFromBody(Block.statements(funcBody))
            }.withTokens(node)
          }
        case _ =>
          node
      }
    }
  }

  /**
    * Process xxxx.call(this, a, b, c)
    * */
  def processCall(n: Node.Node): Node.Node = {
    n.transformAfterSimple {
      case call@Node.CallExpression(expr Dot "call", Seq(_: Node.ThisExpression, a@_*)) =>
        Node.CallExpression (expr, a).withTokens(call)
      case node =>
        node

    }

  }

  // IIFE removal sometimes leaves two scopes directly in one another
  def removeDoubleScope(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      node match {
        case Node.ExpressionStatement(inner: Node.BlockStatement) =>
          //println("Remove Node.ExpressionStatement <- Node.BlockStatement")
          inner
        case Node.BlockStatement(Seq(inner: Node.BlockStatement)) =>
          //println("Remove Node.BlockStatement <- Node.BlockStatement")
          inner
        case func@Node.FunctionExpression(_, _, body, _, _) =>
          //println(s"${nodeClassName(func)} in: ${transformer.parent().map(nodeClassName)}")
          func
        case Node.BlockStatement(seq) =>
          //println(seq.map(nodeClassName).mkString(","))
          node
        case _ =>
          node
      }
    }
  }

  def objectAssign(n: NodeExtended): NodeExtended = {
    // transform Object.assign to individual assignments

    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case t: Node.Program =>
          val newBody = t.body.flatMap {
            case s@Node.ExpressionStatement(Node.CallExpression(Node.Identifier("Object") Dot "assign", Seq(ts@Node.Identifier(sym), x: OObject))) =>
              //println(s"Assign to ${sym.name}")
              // iterate through the object defintion
              // each property replace with an individual assignment statement
              x.properties.collect {
                case p: Node.Property =>
                  //println(s"${p.key}")
                  Node.ExpressionStatement {
                    Node.AssignmentExpression (
                      "=",
                      new Dot(ts, Node.Identifier(propertyKeyName(p.key))),
                      p.value.asInstanceOf[Node.Expression]
                    )
                  }.withTokensDeep(p)
              }
            case s =>
              Some(s)
          }
          t.body = newBody
          t
        case _ =>
          node
      }
    }

    n.copy(top = ret, types = n.types)
  }

  def onTopNode(n: Node.Node => Node.Node): NodeExtended => NodeExtended = { ext =>
    val ret = n(ext.top)
    ext.copy(top = ret.asInstanceOf[Node.Program])
  }

  def apply(n: NodeExtended): NodeExtended = {

    import transform._

    val transforms = Seq[(String, NodeExtended => NodeExtended)](
      "cleanupExports" -> onTopNode(Modules.cleanupExports),
      "inlineImports" -> onTopNode(Modules.inlineImports),
      "handleIncrement" -> onTopNode(handleIncrement),
      "splitMultipleDefinitions" -> onTopNode(Variables.splitMultipleDefinitions),
      "varInitialization" -> onTopNode(Variables.varInitialization),
      "readJSDoc" -> readJSDoc,
      "iife" -> onTopNode(iife), // removes also trailing return within the IIFE construct
      "removeDoubleScope" -> onTopNode(removeDoubleScope), // after iife (often introduced by it)
      "processCall" -> onTopNode(processCall),
      "detectForVars" -> onTopNode(Variables.detectForVars),
      "detectGlobalTemporaries" -> onTopNode(Variables.detectGlobalTemporaries),
      "detectDoubleVars" -> onTopNode(Variables.detectDoubleVars), // before detectVals, so that first access is not turned into val
      "detectVals" -> onTopNode(Variables.detectVals), // before convertConstToFunction
      "detectMethods" -> onTopNode(Variables.detectMethods),
      "convertConstToFunction" -> onTopNode(Variables.convertConstToFunction)
    ) ++ transform.classes.transforms ++ Seq(
      "removeDeprecated" -> onTopNode(Parameters.removeDeprecated),
      "defaultValues" -> onTopNode(Parameters.defaultValues), // if possible, do before type inference, as it is much simpler after it
      "modifications" -> onTopNode(Parameters.modifications),
      "simpleParameters" -> Parameters.simpleParameters _,
      "varInitialization" -> onTopNode(Variables.varInitialization), // already done, but another pass is needed after TransformClasses
      "instanceofImpliedCast" -> Variables.instanceofImpliedCast _,
      "objectAssign" -> objectAssign _,
      "removeVarClassScope" -> onTopNode(removeVarClassScope),
      "types" -> TypesRule.transform _,
      "readTypes" -> ReadTypes.apply _,
      "multipass" -> InferTypes.multipass _,
      "enums" -> TypesRule.transformEnums _,
      "removeTrailingBreak" -> onTopNode(removeTrailingBreak), // before removeTrailingReturn, return may be used to terminate cases
      "removeTrailingReturn" -> onTopNode(removeTrailingReturn), // after inferTypes (returns are needed for inferTypes)
      "inlineConstructorVars" -> Parameters.inlineConstructorVars _, // after type inference, so that all types are already inferred
      "defaultValues" -> onTopNode(Parameters.defaultValues), // do again after inlineConstructorVars, might handle private variables
      "detectVals" -> onTopNode(Variables.detectVals),
      "BoolComparison" -> onTopNode(BoolComparison.apply), // after inferTypes (boolean comparisons may help to infer type as bool)
      "Collections" -> onTopNode(Collections.apply),
      "relations" -> onTopNode(relations)
    )

    transforms.foldLeft(n) { case (t, (opName, op)) =>
      Time(s"step $opName") {
        // TODO: check: is this ever needed? We do not need to keep the old AST around
        val tClone = t.copy(top = t.top.cloneDeep())
        //t.top.figure_out_scope()
        val ret = op(tClone)
        val debug = false
        if (debug) {
          val retString = ScalaOut.output(ret, "").mkString
          println(s"step $opName")
          println(retString)
          println()
        }
        ret
      }
    }
  }
}
