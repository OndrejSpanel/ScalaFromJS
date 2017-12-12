package com.github.opengrabeso

import JsUtils._
import com.github.opengrabeso.esprima._
import _root_.esprima._

import Classes._
import Expressions._

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  import SymbolTypes._
  import Symbols._

  def symbolType(types: SymbolTypes, symbol: Node.Identifier): Option[TypeInfo] = {
    types.get(symbolId(symbol))
  }

  def symbolId(symbol: Node.Identifier): Option[SymbolMapId] = {
    symbol.thedef.flatMap(id)
  }

  def funArg(p: Node.Node): Node.SymbolFunarg = (p: @unchecked) match {
    case p: Node.SymbolFunarg =>
      p
    case p: Node.DefaultAssign =>
      p.left match {
        case a: Node.SymbolFunarg =>
          a
        case _ =>
          throw new UnsupportedOperationException(s"Unexpected argument node ${p.left} in Node.DefaultAssign")
      }
  }

  def symbolFromPar(p: Node.Node): Option[SymbolDef] = p match {
    case p: Node.SymbolFunarg =>
      p.thedef
    case p: Node.DefaultAssign =>
      p.left match {
        case a: Node.SymbolFunarg =>
          a.thedef
        case _ =>
          None
      }
    case _ =>
      None
  }

  // individual sensible transformations

  // convert === to ==
  def relations(n: Node.Node): Node.Node = {
    n.transformAfter { (node, _) =>
      node match {
        case bin@Node.BinaryExpression(_, "===", _) =>
          bin.operator = "=="
          node
        case bin@Node.BinaryExpression(_, "!==", _) =>
          bin.operator = "!="
          node
        case _ =>
          node
      }
    }
  }



  def handleIncrement(n: Node.Node): Node.Node = {

    def substitute(node: Node.Node, expr: Node.Node, op: String) = {
      new Node.Assign {
        fillTokens(this, node)
        left = expr
        operator = op match {
          case "++" => "+="
          case "--" => "-="
        }
        right = new Node.Number {
          fillTokens(this, node)
          value = 1
        }
      }
    }

    // walk the tree, check for increment / decrement
    n.transformAfter { (node, transformer) =>
      def nodeResultDiscarded(n: Node.Node, parentLevel: Int): Boolean = {
        transformer.parent(parentLevel) match {
          case Some(_: Node.SimpleStatement) =>
            true
          case Some(f: Node.For) =>
            // can be substituted inside of for unless used as a condition
            f.init.contains(n) || f.step.contains(n)
          case Some(s: Node.Sequence) =>
            if (s.expressions.last !=n) true
            else if (parentLevel < transformer.stack.length - 2) {
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
        case Node.UnaryExpression(op@UnaryModification(), expr) =>
          if (nodeResultDiscarded(node, 0)) {
            substitute(node, expr, op)
          } else {
            init(new Node.BlockStatement) { i =>

              val operation = Node.SimpleStatement(node)(substitute(node, expr, op))
              node match {
                case _: Node.UnaryPrefix =>
                  val value = Node.SimpleStatement(node)(expr.clone())
                  i.body = js.Array(operation, value)
                case _ /*: Node.UnaryPostfix*/ =>
                  val tempName = "temp"
                  val storeValue = Node.Const(node)(Node.VariableDeclarator.initialized(node)(tempName, expr.clone()))
                  val loadValue = Node.SimpleStatement(node)(Node.Identifier(node)(tempName))
                  i.body = js.Array(storeValue, operation, loadValue)
              }
            }.withTokens(node)
          }
        case _ =>
          node
      }
    }
  }

  def replaceReturnWithStatement(ret: Node.Return) = {
    ret.value.fold[Node.Statement] {
      Node.EmptyStatement(ret)
    } {
      Node.SimpleStatement(ret)
    }
  }

  def walkLastNode(n: Node.Node)(callback: Node.Node => Boolean): Boolean = {
    n match {
      case s: Node.Block =>
        s.body.lastOption.fold(false) { l =>
          val r = callback(l)
          if (!r) walkLastNode(l)(callback)
          r
        }
      case s: Node.SimpleStatement =>
        val r = callback(s.body)
        if (!r) walkLastNode(s.body)(callback)
        r
      case _ =>
        false

    }
  }

  // is this a last node in some scope? (walks also parents)
  def nodeLast(n: Node.Node, parentLevel: Int, transformer: TreeTransformer): Boolean = {
    //println(s"nodeLast ${nodeClassName(n)}:$parentLevel:${transformer.parent(parentLevel).map(nodeClassName)}")
    transformer.parent(parentLevel) match {
      case Some(ss: Node.SimpleStatement) =>
        ss.body == n
      case Some(fun: Node.Lambda) =>
        fun.body.lastOption contains n
      case Some(block: Node.Block) =>
        (block.body.lastOption contains n) && parentLevel < transformer.stack.length - 2 && nodeLast(block, parentLevel + 1, transformer)
      case Some(ii: Node.IfStatement) =>
        (ii.body == n || ii.alternative.contains(n)) && nodeLast(ii, parentLevel + 1, transformer)
      case None =>
        true
      case _ =>
        false
    }
  }

  def removeTrailingReturn(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>

      node match {
        case ret: Node.Return if nodeLast(ret, 0, transformer) =>
          // check if last in a function body
          //println("Remove trailing return")
          replaceReturnWithStatement(ret)
        case _ =>
          node
      }
    }
  }

  def removeReturnFromBody(body: Seq[Node.Statement]): Seq[Node.Statement] = {
    // remove all direct returns
    for (s <- body) yield {
      s.transformBefore { (node, descend, transformer) =>
        node match {
          case _: Node.Lambda =>
            // do not descend into any other functions
            node
          case ret: Node.Return if nodeLast(ret, 0, transformer) =>
            //println(s"Remove return of ${nodeTreeToString(ret)}")
            replaceReturnWithStatement(ret)
          case ret: Node.Return  =>
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
        case sw: Node.Switch =>
          // group conditions with empty body with the following non-empty one
          def conditionGroups(s: Seq[Node.SwitchBranch], ret: Seq[Seq[Node.SwitchBranch]]): Seq[Seq[Node.SwitchBranch]] = s match {
            case Seq() =>
              ret
            case seq =>
              val (empty, nonEmpty) = seq.span (_.body.isEmpty)
              conditionGroups(nonEmpty.drop(1), ret :+ (empty ++ nonEmpty.headOption))
          }

          val body = sw._body.asInstanceOf[js.Array[Node.SwitchBranch]]
          val conditionGrouped = conditionGroups(body, Seq())

          val groupedBody = for (g <- conditionGrouped) yield {
            // all but the last are empty
            def processGroup(e: Seq[Node.SwitchBranch], ret: Node.SwitchBranch): Node.SwitchBranch = {
              def join(c1: Node.SwitchBranch, c2: Node.SwitchBranch) = {
                //println(s"Join ${ScalaOut.outputNode(c1)} ${ScalaOut.outputNode(c2)}")
                assert(c1.body.isEmpty)
                (c1, c2) match {
                  case (case1: Node.Case, case2: Node.Case) =>
                    case2.expression = Node.BinaryExpression(case1.expression) (case1.expression, "|", case2.expression)
                    case2
                  case (case1: Node.Default, case2: Node.Case) =>
                    case1.body = case2.body
                    case1
                  case (_, case2) =>
                    assert(case2.isInstanceOf[Node.Default])
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

          val newBody = new mutable.ArrayBuffer[Node.Statement]
          for (s <- groupedBody) {
            s.body.lastOption match {
              case Some(_: Node.Break) =>
                s.body = s.body.dropRight(1)
                newBody append s
              case Some(_: Node.Throw) =>
                newBody append s
              case Some(_: Node.Return) =>
                newBody append s
              case _  =>
                // fall through branches - warn
                if (s != groupedBody.last) {
                  s.body = s.body ++ js.Array(unsupported("Missing break", s))
                }
                newBody append s
            }
          }
          sw.body = newBody.toJSArray
          sw
        case _ =>
          node
      }
    }
  }

  def readJSDoc(n: NodeExtended): NodeExtended = {
    var commentsParsed = Set.empty[Int]

    val declBuffer = new mutable.ArrayBuffer[(SymbolDef, TypeDesc)]()

    n.top.walk { node =>
      node match {
        case f: DefFun =>
          for {
            start <- f.start
            commentToken <- start.comments_before.lastOption
          } {
            val comment = commentToken.value.asInstanceOf[String]
            if (!(commentsParsed contains commentToken.pos)) {
              commentsParsed += commentToken.pos
              for (commentLine <- comment.lines) {
                // match anything in form:

                val JSDocParam = """@param +([^ ]+) +\{([^ ]+)\}""".r.unanchored // @param name {type}
                val JSDocReturn = """@return +\{([^ ]+)\}""".r.unanchored // @return {type}
                commentLine match {
                  case JSDocParam(name, tpe) =>
                    // find a corresponding symbol
                    val sym = f.argnames.find(_.name == name)
                    for {
                      s <- sym
                      td <- s.thedef
                    } {
                      declBuffer append td -> parseType(tpe)
                    }
                  case JSDocReturn(tpe) =>
                    for {
                      s <- f.name
                      td <- s.thedef
                    } {
                      declBuffer append td -> parseType(tpe)
                    }
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
        classInfo.mostDerived(c1.name, c2.name).fold[TypeDesc](any)(ClassType)
      }

      def commonBase(c1: ClassType, c2: ClassType) = {
        //println("commonBase")
        classInfo.commonBase(c1.name, c2.name).fold[TypeDesc](any)(ClassType)
      }
    }

    implicit val classPos: (SymbolMapId) => Int = classes.classPos

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
    def unapply(arg: Node.Node)(implicit ctx: ExpressionTypeContext): Option[Option[TypeInfo]] = {
      Some(expressionType(arg))
    }
  }

  def expressionType(n: Node.Node, log: Boolean = false)(implicit ctx: ExpressionTypeContext): Option[TypeInfo] = {
    import ctx._
    //println(s"  type ${nodeClassName(n)}: ${ScalaOut.outputNode(n)}")

    /*
    * @param certain when context indicates it is a class name (in new or instanceof)
    * */
    def typeInfoFromClassSym(classSym: SymbolDef, certain: Boolean = false): Option[TypeInfo] = {
      // is the symbol a class?
      for {
        cls <- id(classSym)
        if certain || ctx.classes.get(cls).isDefined
      } yield {
        TypeInfo.target(ClassType(cls))
      }
    }

    def typeInfoFromClassDef(classDef: Option[Node.DefClass]) = {
      classDef.flatMap(_.name).flatMap(_.thedef).flatMap(typeInfoFromClassSym(_))
    }

    n match {
      case s: Node.Super =>
        val sup = findSuperClass(s.scope)
        //println(s"super scope $sup")
        sup.map(t => TypeInfo.target(ClassType(t)))

      case t: Node.This =>
        val thisScope = findThisClass(t.scope) // TODO: consider this in a function
        //println(s"this scope ${t.scope.map(_.nesting)}")
        typeInfoFromClassDef(thisScope)
        //println(s"this def scope $cls")

      case Node.Identifier("undefined", _, thedef)  =>
        // not allowing undefined overrides
        None // Some(TypeInfo(AnyType, NoType))

      case Node.Identifier(symDef) =>
        // if the symbol is a class name, use it as a class type directly
        val rt = types.get(symDef).orElse(typeInfoFromClassSym(symDef))
        //println(s"    Sym ${symDef.name} type $rt")
        rt

      case expr Dot name =>
        val exprType = expressionType(expr, log)(ctx)
        if (log) println(s"type of member $expr.$name, class $exprType")
        for {
          TypeDecl(ClassType(callOn)) <- exprType
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(MemberId(c, name)))
        } yield {
          if (log) println(s"type of member $c.$name as $r")
          r
        }

      case expr Node.Sub name =>
        //println(s"Infer type of array item $name, et ${expressionType(expr)(ctx)}")
        expressionType(expr, log)(ctx) match {
          case Some(TypeDecl(ArrayType(item))) =>
            val r = TypeInfo.target(item)
            if (log) println(s"type of array $expr.$name as $r")
            Some(r)
          case Some(TypeDecl(MapType(item))) =>
            val r = Some(TypeInfo.target(item))
            if (log) println(s"type of map $expr.$name as $r")
            r
          case _ =>
            None
        }

      case a: AArray =>
        val elementTypes = a.elements.map(expressionType(_, log)(ctx))
        val elType = elementTypes.reduceOption(typeUnionOption).flatten
        if (log) {
          println(s"  elementTypes $elementTypes => $elType")
        }
        Some(elType.map(_.map(ArrayType)).getOrElse(TypeInfo(ArrayType(AnyType), ArrayType(NoType))))
      case a: OObject =>
        Some(TypeInfo.target(ObjectOrMap))
      case _: Node.Number =>
        Some(TypeInfo.target(number))
      case _: Node.String =>
        Some(TypeInfo.target(string))
      case _: Node.Boolean =>
        Some(TypeInfo.target(boolean))

      // Array.isArray( name ) ? name : [name]
      case Node.Conditional(Node.CallExpression(Node.Identifier("Array") Dot "isArray", isArrayArg), exprTrue, exprFalse) =>
        val exprType = expressionType(isArrayArg, log)
        //println(s"ExprType $exprType of Array.isArray( name ) ? name : [name] for $n1")
        if(exprType.exists(_.declType.isInstanceOf[ArrayType])) {
          expressionType(exprTrue, log)
        } else {
          expressionType(exprFalse, log)
        }
      case Node.Conditional(_, te, fe) =>
        val t = expressionType(te, log)
        val f = expressionType(fe, log)
        val ret = typeUnionOption(t, f)
        if (log) println(s"Node.Conditional $te:$t / $fe:$f = $ret")
        ret

      case Node.BinaryExpression(expr, `asinstanceof`, Node.Identifier(cls)) =>
        typeInfoFromClassSym(cls, true)

      case Node.BinaryExpression(left, op, right) =>
        // sometimes operation is enough to guess an expression type
        // result of any arithmetic op is a number
        op match {
          case IsArithmetic() => Some(TypeInfo.target(number))
          case IsComparison() => Some(TypeInfo.target(boolean))
          case "+" =>
            val typeLeft = expressionType(left, log)(ctx)
            val typeRight = expressionType(right, log)(ctx)
            // string + anything is a string
            if (typeLeft == typeRight) typeLeft
            else if (typeLeft.exists(_.target == string) || typeRight.exists(_.target == string)) Some(TypeInfo.target(string))
            else None
          case IsBoolean() =>
            // boolean with the same type is the same type
            val typeLeft = expressionType(left, log)(ctx)
            val typeRight = expressionType(right, log)(ctx)
            if (typeLeft == typeRight) typeLeft
            else None
          case _ =>
            None
        }
      case Node.New(Node.Identifier(call), _*) =>
        typeInfoFromClassSym(call, true)
      case Node.New(Node.Identifier(pack) Dot call, _*) =>
        // TODO: check if call is a known symbol and use it
        // TODO: handle package names properly
        Some(TypeInfo.both(ClassType(SymbolMapId(call, 0))))
      case Node.CallExpression(Node.Identifier(call), _*) =>
        val tid = id(call)
        if (log)  println(s"Infer type of call ${call.name}:$tid as ${types.get(tid)}")
        types.get(tid).map(callReturn)

      case Node.CallExpression(cls Dot name, _*) =>
        val exprType = expressionType(cls, log)(ctx)
        if (log) println(s"Infer type of member call $cls.$name class $exprType")
        for {
          TypeDecl(ClassType(callOn)) <- exprType
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(MemberId(c, name)))
        } yield {
          if (log) println(s"  Infer type of member call $c.$name as $r")
          callReturn(r)
        }
      case seq: Node.Sequence =>
        expressionType(seq.expressions.last, log)(ctx)
      case Node.SimpleStatement(ExpressionType(t)) =>
        t

      case Node.BlockStatement( _ :+ ExpressionType(last)) =>
        last
      case fun@Node.Lambda(args, body) =>
        val returnType = transform.InferTypes.scanFunctionReturns(fun)(ctx)
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

  def isReadOnlyProperty(cls: Node.DefClass, name: String): Option[Node.Node] = {
    var getter = Option.empty[Node.Node]
    var setter = false
    cls.properties.filter(p => propertyName(p) == name).foreach {
      case Node.MethodDefinition(Node.SymbolName(p), _) =>
        //println(s"  function $p")
        //getter = true
      case ObjectKeyVal(p, value) =>
        //println(s"  keyval $p")
        getter = Some(value)
      case s: Node.ObjectSetter =>
        //println(s"  setter ${s.key.name}")
        setter = true
      case Node.ObjectGetter(p, Node.Function(Seq(), Seq(Node.SimpleStatement(init)))) =>
        // check
        //println(s"  getter init $p}")
        getter = Some(init)
      case _ =>
    }
    if (!setter) getter
    else None
  }

  def listPrototypeMemberNames(cls: Node.DefClass): Seq[String] = {
    var existingMembers = Seq.empty[String]

    def addAccessor(s: Node.ObjectSetterOrGetter) = {
      s.key match {
        case Node.Identifier(name) =>
          existingMembers = existingMembers :+ name
        case _ =>
      }
    }

    cls.properties.foreach {
      case Node.MethodDefinition(Node.SymbolName(p), _) =>
        existingMembers = existingMembers :+ p
      case ObjectKeyVal(p, _) =>
        existingMembers = existingMembers :+ p
      case s: Node.ObjectSetter =>
        addAccessor(s)
      case s: Node.ObjectGetter =>
        addAccessor(s)
      case _ =>
    }
    existingMembers
  }

  def listDefinedClassMembers(node: NodeExtended) = {
    var listMembers = ClassInfo()
    node.top.walk {
      case cls@Node.DefClass(Defined(Node.Identifier(_, _, Defined(clsSym))), base, _) =>
        for (clsId <- id(clsSym)) {
          for {
            Node.Identifier(parent) <- base
            parentId <- id(parent)
          } {
            //println(s"Add parent $parent for $clsSym")
            listMembers = listMembers.copy(parents = listMembers.parents + (clsId -> parentId))
          }
          val members = listPrototypeMemberNames(cls)

          // list data members
          //println(s"varMembers for $cls")
          val varMembers = for {
            inlineBody <- findInlineBody(cls).toSeq
            VarName(member) <- inlineBody.value.body
          } yield member

          // TODO: maybe parMembersInline is enough?

          def listParameters(method: Option[Node.MethodDefinition]) = method.toSeq.flatMap(_.value.argnames.flatMap(Transform.symbolFromPar))

          val parMembers = listParameters(findConstructor(cls)).map(_.name)
          val parMembersInline = for {
            arg <- listParameters(findInlineBody(cls))
            argId = clsId.copy(name = arg.name) // arg may be constructed by us - may miss symbol/token information
            //_ = println(s"Arg $argId from cls $clsId hints ${node.types.hints.get(argId)}")
            if !node.types.hints.get(argId).contains(IsConstructorParameter) && !arg.name.endsWith(parSuffix)
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
          case Node.Return(Defined(x)) => // regular return
            Some(x)
          case Node.SimpleStatement(x) => // elided trailing return
            Some(x)
          case _ =>
            None
        }
      }

      def unapply(arg: Seq[Node.Statement]) = arg match {
        case Seq(defClass@Node.DefClass(Defined(c), _, _), ReturnedExpression(r : Node.Identifier)) if c.thedef == r.thedef =>
          Some(defClass, c)
        case _ => None
      }
    }

    n.transformAfter { (node, _) =>
      node match {
        case Node.VariableDeclaration(Node.VariableDeclarator(Node.Identifier(sym), Defined(Node.BlockStatement(DefineAndReturnClass(defClass, r))))) if sym.name == r.name =>
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
        case Node.CallExpression(Node.Lambda(args1, funcBody), args2@_*) if args1.isEmpty && args2.isEmpty =>
          Some(funcBody)
        case _ => None

      }
    }

    n.transformAfter { (node, _) =>
      node match {
        case IIFE(funcBody) =>
          new Node.BlockStatement {
            fillTokens(this, node)
            this.body = removeReturnFromBody(funcBody).toJSArray
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
      case call@Node.CallExpression(expr Dot "call", _: Node.This, a@_* ) =>
        new Node.CallExpression {
          fillTokens(this, call)
          this.expression = expr
          this.args = a.toJSArray
        }
      case node =>
        node

    }

  }

  // IIFE removal sometimes leaves two scopes directly in one another
  def removeDoubleScope(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      node match {
        case Node.SimpleStatement(inner: Node.BlockStatement) =>
          //println("Remove Node.SimpleStatement <- Node.BlockStatement")
          inner
        case Node.BlockStatement(Seq(inner: Node.BlockStatement)) =>
          //println("Remove Node.BlockStatement <- Node.BlockStatement")
          inner
        case func@Node.Function(_, Seq(Node.BlockStatement(body))) =>
          //println("Remove Node.Function <- Node.BlockStatement")
          func.body = body.toJSArray
          func
        case func@Node.Accessor(_, Seq(Node.BlockStatement(body))) =>
          //println("Remove Node.Accessor <- Node.BlockStatement")
          func.body = body.toJSArray
          func
        case func@Node.Lambda(_, body) =>
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
            case s@Node.SimpleStatement(Node.CallExpression(Node.Identifier("Object") Dot "assign", ts@Node.Identifier(sym), x: OObject)) =>
              //println(s"Assign to ${sym.name}")
              // iterate through the object defintion
              // each property replace with an individual assignment statement
              x.properties.collect {
                case p: ObjectKeyVal =>
                  //println(s"${p.key}")
                  Node.SimpleStatement(p) {
                    new Node.Assign {
                      fillTokens(this, p)
                      left = new Dot {
                        expression = ts
                        property = p.key
                        fillTokens(this, p)
                        Node.Identifier(p)(p.key)
                      }
                      operator = "="
                      right = p.value
                    }
                  }
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

    val transforms = Seq[NodeExtended => NodeExtended](
      onTopNode(Modules.cleanupExports),
      onTopNode(Modules.inlineImports),
      onTopNode(handleIncrement),
      onTopNode(Variables.splitMultipleDefinitions),
      onTopNode(Variables.varInitialization),
      readJSDoc,
      onTopNode(iife), // removes also trailing return within the IIFE construct
      onTopNode(removeDoubleScope), // after iife (often introduced by it)
      onTopNode(processCall),
      onTopNode(Variables.detectForVars),
      onTopNode(Variables.detectDoubleVars), // before detectVals, so that first access is not turned into val
      onTopNode(Variables.detectVals), // before convertConstToFunction
      onTopNode(Variables.detectMethods),
      onTopNode(Variables.convertConstToFunction)
    ) ++ transform.classes.transforms ++ Seq(
      onTopNode(Parameters.removeDeprecated),
      onTopNode(Parameters.defaultValues),
      onTopNode(Parameters.modifications),
      Parameters.simpleParameters _,
      onTopNode(Variables.varInitialization), // already done, but another pass is needed after TransformClasses
      Variables.instanceofImpliedCast _,
      objectAssign _,
      onTopNode(removeVarClassScope),
      InferTypes.multipass _,
      onTopNode(removeTrailingBreak), // before removeTrailingReturn, return may be used to terminate cases
      onTopNode(removeTrailingReturn), // after inferTypes (returns are needed for inferTypes)
      Parameters.inlineConstructorVars _, // after type inference, so that all types are already inferred
      onTopNode(Variables.detectVals),
      onTopNode(BoolComparison.apply), // after inferTypes (boolean comparisons may help to infer type as bool)
      onTopNode(Collections.apply),
      onTopNode(relations)
    )

    transforms.zipWithIndex.foldLeft(n) { (t,op) =>
      Time(s"step ${op._2}") {
        t.top.figure_out_scope()
        op._1(t)
      }
    }
  }
}
