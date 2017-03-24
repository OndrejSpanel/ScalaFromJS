package com.github.opengrabeso

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._

import scala.collection.mutable
import scala.scalajs.js
import js.JSConverters._
import scala.language.implicitConversions

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  import SymbolTypes._

  object AST_Extended {
    def noTypes = SymbolTypes()
  }
  case class AST_Extended(top: AST_Toplevel, types: SymbolTypes)

  // individual sensible transformations

  // convert === to ==
  def relations(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case bin@AST_Binary(_, "===", _) =>
          bin.operator = "=="
          node
        case bin@AST_Binary(_, "!==", _) =>
          bin.operator = "!="
          node
        case _ =>
          node
      }
    }
  }



  def handleIncrement(n: AST_Node): AST_Node = {

    def substitute(node: AST_Node, expr: AST_Node, op: String) = {
      new AST_Assign {
        fillTokens(this, node)
        left = expr
        operator = op match {
          case "++" => "+="
          case "--" => "-="
        }
        right = new AST_Number {
          fillTokens(this, node)
          value = 1
        }
      }
    }

    // walk the tree, check for increment / decrement
    n.transformAfter { (node, transformer) =>
      def nodeResultDiscarded(n: AST_Node, parentLevel: Int): Boolean = {
        transformer.parent(parentLevel).nonNull match {
          case Some(_: AST_SimpleStatement) =>
            true
          case Some(f: AST_For) =>
            // can be substituted inside of for unless used as a condition
            f.init.contains(n) || f.step.contains(n)
          case Some(s: AST_Seq ) =>
            if (s.cdr !=n) true
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
        case AST_Unary(op@UnaryModification(), expr) =>
          if (nodeResultDiscarded(node, 0)) {
            substitute(node, expr, op)
          } else {
            new AST_BlockStatement {
              fillTokens(this, node)

              def operation = new AST_SimpleStatement {
                fillTokens(this, node)
                body = substitute(node, expr, op)
              }
              def value = new AST_SimpleStatement {
                fillTokens(this, node)
                body = expr.clone()
              }
              val tempName = "temp"
              def storeValue = new AST_SimpleStatement {
                fillTokens(this, node)
                body = new AST_Let {
                  fillTokens(this, node)
                  definitions = js.Array(new AST_VarDef {
                    fillTokens(this, node)
                    name = new AST_SymbolVar {
                      fillTokens(this, node)
                      name = tempName
                    }
                    value = expr.clone()
                  })
                }
              }

              def loadValue = new AST_SimpleStatement {
                fillTokens(this, node)
                body = new AST_SymbolRef {
                  fillTokens(this, node)
                  name = tempName
                }
              }

              node match {
                case _: AST_UnaryPrefix =>
                  this.body = js.Array(operation, value)
                case _ /*: AST_UnaryPostfix*/ =>
                  this.body = js.Array(storeValue, operation, loadValue)
              }
            }
          }
        case _ =>
          node
      }
    }
  }

  def replaceReturnWithStatement(ret: AST_Return) = {
    ret.value.nonNull.fold[AST_Statement] {
      new AST_EmptyStatement {
        fillTokens(this, ret)
      }
    } { v =>
      new AST_SimpleStatement {
        fillTokens(this, ret)
        body = v

      }
    }
  }

  def walkLastNode(n: AST_Node)(callback: AST_Node => Boolean): Boolean = {
    n match {
      case s: AST_Block =>
        s.body.lastOption.fold(false) { l =>
          val r = callback(l)
          if (!r) walkLastNode(l)(callback)
          r
        }
      case s: AST_SimpleStatement =>
        val r = callback(s.body)
        if (!r) walkLastNode(s.body)(callback)
        r
      case _ =>
        false

    }
  }

  // is this a last node in some scope? (walks also parents)
  def nodeLast(n: AST_Node, parentLevel: Int, transformer: TreeTransformer): Boolean = {
    //println(s"nodeLast ${nodeClassName(n)}:$parentLevel:${transformer.parent(parentLevel).map(nodeClassName)}")
    transformer.parent(parentLevel).nonNull match {
      case Some(ss: AST_SimpleStatement) =>
        ss.body == n
      case Some(fun: AST_Lambda) =>
        fun.body.lastOption contains n
      case Some(block: AST_Block) =>
        (block.body.lastOption contains n) && parentLevel < transformer.stack.length - 2 && nodeLast(block, parentLevel + 1, transformer)
      case Some(ii: AST_If) =>
        (ii.body == n || ii.alternative.contains(n)) && nodeLast(ii, parentLevel + 1, transformer)
      case None =>
        true
      case _ =>
        false
    }
  }

  def removeTrailingReturn(n: AST_Node): AST_Node = {
    n.transformAfter { (node, transformer) =>

      node match {
        case ret: AST_Return if nodeLast(ret, 0, transformer) =>
          // check if last in a function body
          //println("Remove trailing return")
          replaceReturnWithStatement(ret)
        case _ =>
          node
      }
    }
  }

  def removeReturnFromBody(body: Seq[AST_Statement]): Seq[AST_Statement] = {
    // remove all direct returns
    for (s <- body) yield {
      s.transformBefore { (node, descend, transformer) =>
        node match {
          case _: AST_Lambda =>
            // do not descend into any other functions
            node
          case ret: AST_Return if nodeLast(ret, 0, transformer) =>
            //println(s"Remove return of ${nodeTreeToString(ret)}")
            replaceReturnWithStatement(ret)
          case ret: AST_Return  =>
            //println(s"No remove return of ${nodeTreeToString(ret)}")
            node
          case _ =>
            descend(node, transformer)
            node
        }

      }
    }
  }

  def removeTrailingBreak(n: AST_Node): AST_Node = {

    n.transformAfter { (node, transformer) =>

      node match {
        case sw: AST_Switch =>
          // group conditions with empty body with the following non-empty one
          def conditionGroups(s: Seq[AST_SwitchBranch], ret: Seq[Seq[AST_SwitchBranch]]): Seq[Seq[AST_SwitchBranch]] = s match {
            case Seq() =>
              ret
            case seq =>
              val (empty, nonEmpty) = seq.span (_.body.isEmpty)
              conditionGroups(nonEmpty.drop(1), ret :+ (empty ++ nonEmpty.headOption))
          }

          val body = sw._body.asInstanceOf[js.Array[AST_SwitchBranch]]
          val conditionGrouped = conditionGroups(body, Seq())

          val groupedBody = for (g <- conditionGrouped) yield {
            // all but the last are empty
            def processGroup(e: Seq[AST_SwitchBranch], ret: AST_SwitchBranch): AST_SwitchBranch = {
              def join(c1: AST_SwitchBranch, c2: AST_SwitchBranch) = {
                //println(s"Join ${ScalaOut.outputNode(c1)} ${ScalaOut.outputNode(c2)}")
                assert(c1.body.isEmpty)
                (c1, c2) match {
                  case (case1: AST_Case, case2: AST_Case) =>
                    case2.expression = new AST_Binary {
                      fillTokens(this, case1.expression)
                      left = case1.expression
                      operator = "|"
                      right = case2.expression
                    }
                    case2
                  case (case1: AST_Default, case2: AST_Case) =>
                    case1.body = case2.body
                    case1
                  case (_, case2) =>
                    assert(case2.isInstanceOf[AST_Default])
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

          val newBody = new mutable.ArrayBuffer[AST_Statement]
          for (s <- groupedBody) {
            s.body.lastOption match {
              case Some(_: AST_Break) =>
                s.body = s.body.dropRight(1)
                newBody append s
              case Some(_: AST_Throw) =>
                newBody append s
              case Some(_: AST_Return) =>
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

  def readJSDoc(n: AST_Extended): AST_Extended = {
    var commentsParsed = Set.empty[Int]

    val declBuffer = new mutable.ArrayBuffer[(SymbolDef, TypeDesc)]()

    n.top.walk { node =>
      node match {
        case f: AST_Defun =>
          for {
            start <- f.start.nonNull
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
                      td <- s.thedef.nonNull
                    } {
                      declBuffer append td -> parseType(tpe)
                    }
                  case JSDocReturn(tpe) =>
                    for {
                      s <- f.name.nonNull
                      td <- s.thedef.nonNull
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
    AST_Extended(n.top, n.types ++ SymbolTypes(declBuffer))
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
  case class ExpressionTypeContext(types: Ref[SymbolTypes], classInfo: ClassInfo, classes: Map[String, AST_DefClass]) {
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

  }

  object TypeDecl {
    def unapply(arg: TypeInfo) = Some(arg.declType)
  }

  def callReturn(funType: TypeInfo): TypeInfo = funType.declType match {
    case FunctionType(ret, _) =>
      //println(s"callReturn $ret")
      TypeInfo.target(ret)
    case _ => funType
  }

  def expressionType(n: AST_Node)(ctx: ExpressionTypeContext): Option[TypeInfo] = {
    import ctx._
    //println(s"  type ${nodeClassName(n)}: ${ScalaOut.outputNode(n)}")

    n match {
      case s: AST_Super =>
        val sup = findSuperClass(s.scope.nonNull)(ctx)
        //println(s"super scope $sup")
        sup.map(t => TypeInfo.target(ClassType(t)))

      case t: AST_This =>
        val thisScope = findThisClass(t.scope.nonNull) // TODO: consider this in a function
        //println(s"this scope ${t.scope.map(_.nesting)}")
        val cls = thisScope.flatMap(_.name.nonNull).map(_.name)
        //println(s"this def scope $cls")
        cls.map(t => TypeInfo.target(ClassType(t)))

      case AST_SymbolRef("undefined", _, thedef)  =>
        // not allowing undefined overrides
        None // Some(TypeInfo(AnyType, NoType))

      case AST_SymbolRefDef(symDef) =>
        // if the symbol is a class name, use it as a class type directly
        val rt = types.get(symDef).orElse {
          val r = classes.get(symDef.name).flatMap(_.name.nonNull.map(_.name)).map(c => TypeInfo.target(ClassType(c)))
          r
        }
        //println(s"Sym ${symDef.name} type $rt")
        rt

      case AST_Dot(cls, name) =>
        //println(s"Infer type of member $name, et ${expressionType(cls)(ctx)}")
        for {
          TypeDecl(ClassType(callOn)) <- expressionType(cls)(ctx)
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(c), name)
        } yield {
          //println(s"Infer type of member $c.$name as $r")
          r
        }
      case _: AST_Array =>
        // TODO: check inside of the array
        Some(TypeInfo.target(ArrayType(NoType)))
      case _: AST_Number =>
        Some(TypeInfo.target(number))
      case _: AST_String =>
        Some(TypeInfo.target(string))
      case _: AST_Boolean =>
        Some(TypeInfo.target(boolean))
      case tern: AST_Conditional =>
        val t1 = expressionType(tern.consequent)(ctx)
        val t2 = expressionType(tern.consequent)(ctx)
        typeUnionOption(t1, t2)
      case AST_Binary(left, op, right) =>
        // sometimes operation is enough to guess an expression type
        // result of any arithmetic op is a number
        op match {
          case IsArithmetic() => Some(TypeInfo.target(number))
          case IsComparison() => Some(TypeInfo.target(boolean))
          case "+" =>
            val typeLeft = expressionType(left)(ctx)
            val typeRight = expressionType(right)(ctx)
            // string + anything is a string
            if (typeLeft == typeRight) typeLeft
            else if (typeLeft.exists(_.target == string) || typeRight.exists(_.target == string)) Some(TypeInfo.target(string))
            else None
          case IsBoolean() =>
            // boolean with the same type is the same type
            val typeLeft = expressionType(left)(ctx)
            val typeRight = expressionType(right)(ctx)
            if (typeLeft == typeRight) typeLeft
            else None
          case _ =>
            None
        }
      case AST_New(AST_SymbolRefDef(call), _*) =>
        Some(TypeInfo.target(ClassType(call.name)))
      case AST_Call(AST_SymbolRefDef(call), _*) =>
        val tid = id(call)
        //println(s"Infer type of call ${call.name}:$tid as ${types.get(tid)}")
        types.get(tid).map(callReturn)

      case AST_Call(cls AST_Dot name, _*) =>
        //println(s"Infer type of member call $name")
        for {
          TypeDecl(ClassType(callOn)) <- expressionType(cls)(ctx)
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(c), name)
        } yield {
          //println(s"  Infer type of member call $c.$name as $r")
          callReturn(r)
        }
      case seq: AST_Seq =>
        expressionType(seq.cdr)(ctx)
      case _ =>
        None

    }
  }

  def classListHarmony(n: AST_Extended) = {
    var classes = Map.empty[String, AST_DefClass]
    n.top.walk {
      case d: AST_DefClass =>
        for (name <- d.name) {
          classes += name.name -> d
        }
        true
      case _ : AST_Toplevel =>
        false
      case _ =>
        false
    }
    classes
  }

  def listPrototypeMemberNames(cls: AST_DefClass): Seq[String] = {
    var existingMembers = Seq.empty[String]

    def addAccessor(s: AST_ObjectSetterOrGetter) = {
      s.key match {
        case AST_SymbolRefName(name) =>
          existingMembers = existingMembers :+ name
        case _ =>
      }
    }

    cls.properties.foreach {
      case AST_ConciseMethod(AST_SymbolName(p), _) =>
        existingMembers = existingMembers :+ p
      case AST_ObjectKeyVal(p, _) =>
        existingMembers = existingMembers :+ p
      case s: AST_ObjectSetter =>
        addAccessor(s)
      case s: AST_ObjectGetter =>
        addAccessor(s)
      case _ =>
    }
    existingMembers
  }

  def listDefinedClassMembers(node: AST_Node) = {
    var listMembers = ClassInfo()
    node.walk {
      case cls@AST_DefClass(Defined(AST_SymbolName(clsName)), base, _) =>
        for (AST_SymbolName(parent) <- base) {
          //println(s"Add parent $parent for $clsName")
          listMembers = listMembers.copy(parents = listMembers.parents + (clsName -> parent))
        }
        val members = listPrototypeMemberNames(cls)

        // list data members
        val varMembers = for (VarName(member) <- classInlineBody(cls).body) yield member

        val parMembers = for {
          constructor <- findConstructor(cls).toSeq
          args <- constructor.value.argnames
          argName = args.name
          if !argName.endsWith(parSuffix)
        } yield {
          argName
        }

        //println(s"$clsName: parMembers $parMembers")
        val clsMembers = clsName -> (members ++ varMembers ++ parMembers).distinct

        listMembers = listMembers.copy(members = listMembers.members + clsMembers)
        //println(s"listMembers $listMembers (++ $clsMembers)")
        false
      case _ =>
        false
    }
    listMembers
  }

  def listClassMembers(node: AST_Node) = {
    SymbolTypes.stdClassInfo ++ listDefinedClassMembers(node)
  }

  // get rid of transform var XXX = function(){ function XXX(); return XXX; }()
  def removeVarClassScope(n: AST_Node): AST_Node = {
    object DefineAndReturnClass {
      private object ReturnedExpression {
        def unapply(arg: AST_Node) = arg match {
          case AST_Return(Defined(x)) => // regular return
            Some(x)
          case AST_SimpleStatement(x) => // elided trailing return
            Some(x)
          case _ =>
            None
        }
      }

      def unapply(arg: Seq[AST_Statement]) = arg match {
        case Seq(defClass@AST_DefClass(Defined(c), _, _), ReturnedExpression(r : AST_SymbolRef)) if c.thedef == r.thedef =>
          Some(defClass, c)
        case _ => None
      }
    }

    n.transformAfter { (node, _) =>
      node match {
        case AST_Definitions(AST_VarDef(AST_SymbolDef(sym), Defined(AST_BlockStatement(DefineAndReturnClass(defClass, r))))) if sym.name == r.name =>
          defClass
        case _ =>
          node
      }
    }
  }

  def iife(n: AST_Node): AST_Node = {
    //println(nodeTreeToString(n.top))

    // "Immediately-invoked function expression"
    object IIFE {
      def unapply(arg: AST_Node) = arg match {
        case AST_Call(AST_Lambda(args1, funcBody), args2@_*) if args1.isEmpty && args2.isEmpty =>
          Some(funcBody)
        case _ => None

      }
    }

    n.transformAfter { (node, _) =>
      node match {
        case IIFE(funcBody) =>
          new AST_BlockStatement {
            fillTokens(this, node)
            this.body = removeReturnFromBody(funcBody).toJSArray
          }
        case _ =>
          node
      }
    }
  }

  // IIFE removal sometimes leaves two scopes directly in one another
  def removeDoubleScope(n: AST_Node): AST_Node = {
    n.transformAfter { (node, transformer) =>
      node match {
        case AST_SimpleStatement(inner: AST_BlockStatement) =>
          //println("Remove AST_SimpleStatement <- AST_BlockStatement")
          inner
        case AST_BlockStatement(Seq(inner: AST_BlockStatement)) =>
          //println("Remove AST_BlockStatement <- AST_BlockStatement")
          inner
        case func@AST_Function(_, Seq(AST_BlockStatement(body))) =>
          //println("Remove AST_Function <- AST_BlockStatement")
          func.body = body.toJSArray
          func
        case func@AST_Accessor(_, Seq(AST_BlockStatement(body))) =>
          //println("Remove AST_Accessor <- AST_BlockStatement")
          func.body = body.toJSArray
          func
        case func@AST_Lambda(_, body) =>
          //println(s"${nodeClassName(func)} in: ${transformer.parent().map(nodeClassName)}")
          func
        case AST_BlockStatement(seq) =>
          //println(seq.map(nodeClassName).mkString(","))
          node
        case _ =>
          node
      }
    }
  }

  def objectAssign(n: AST_Extended): AST_Extended = {
    // transform Object.assign to individual assignments

    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case t: AST_Toplevel =>
          val newBody = t.body.flatMap {
            case s@AST_SimpleStatement(AST_Call(AST_SymbolRefName("Object") AST_Dot "assign", ts@AST_SymbolRefDef(sym), x: AST_Object)) =>
              //println(s"Assign to ${sym.name}")
              // iterate through the object defintion
              // each property replace with an individual assignment statement
              x.properties.collect {
                case p: AST_ObjectKeyVal =>
                  //println(s"${p.key}")
                  new AST_SimpleStatement {
                    fillTokens(this, p)
                    body = new AST_Assign {
                      fillTokens(this, p)
                      left = new AST_Dot {
                        expression = ts
                        property = p.key
                        fillTokens(this, p)
                        new AST_SymbolRef {
                          fillTokens(this, p)
                          name = p.key
                        }
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

    AST_Extended(ret, n.types)

  }

  def onTopNode(n: AST_Node => AST_Node): AST_Extended => AST_Extended = { ext =>
    val ret = n(ext.top)
    AST_Extended(ret.asInstanceOf[AST_Toplevel], ext.types)
  }

  def apply(n: AST_Toplevel): AST_Extended = {

    import transform._

    val transforms = Seq(
      onTopNode(handleIncrement),
      onTopNode(Variables.varInitialization),
      readJSDoc _,
      onTopNode(iife), // removes also trailing return within the IIFE construct
      onTopNode(removeDoubleScope), // after iife (often introduced by it)
      onTopNode(Variables.detectVals), // before convertConstToFunction
      onTopNode(Variables.detectMethods),
      onTopNode(Variables.convertConstToFunction)
    ) ++ TransformClasses.transforms ++ Seq(
      onTopNode(Parameters.removeDeprecated),
      onTopNode(Parameters.defaultValues),
      onTopNode(Variables.varInitialization), // already done, but another pass is needed after TransformClasses
      objectAssign _,
      onTopNode(removeVarClassScope),
      InferTypes.multipass _,
      onTopNode(removeTrailingBreak), // before removeTrailingReturn, return may be used to terminate cases
      onTopNode(removeTrailingReturn), // after inferTypes (returns are needed for inferTypes)
      Parameters.inlineConstructorVars _, // after type inference, so that all types are already inferred
      onTopNode(Variables.detectVals),
      onTopNode(relations)
    )

    transforms.zipWithIndex.foldLeft(AST_Extended(n, SymbolTypes())) { (t,op) =>
      val now = System.nanoTime()
      t.top.figure_out_scope()
      val r = op._1(t)
      val duration = System.nanoTime() - now
      //println(s"step ${op._2}, duration ${duration / 1000000}")
      r
    }
  }
}
