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

  object UnaryModification {
    def unapply(arg: String): Boolean = arg == "++" || arg == "--"
  }

  // individual sensible transformations

  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Extended): AST_Extended = {
    // walk the tree, check for possible val replacements and perform them
    val ret = n.top.transformAfter {(node, _) =>
      node match {
        case AST_Var(varDef@AST_VarDef(varName, value)) if value.nonNull.nonEmpty => // var with init - search for a modification
          varName.thedef.fold(node) { df =>
            assert(df.name == varName.name)
            // check if any reference is assignment target
            val assignedInto = df.references.exists { ref =>
              //println(s"Reference to ${df.name} in scope ${ref.scope.get.nesting}")
              assert(ref.thedef.exists(_ == df))
              ref.scope.exists { s =>
                var detect = false
                s.walk {
                  case ss: AST_Scope => ss != ref.scope // do not descend into any other scopes, they are listed in references if needed
                  case AST_Assign(AST_SymbolRef(_, _, `df`), _, _) =>
                    //println(s"  Detected assignment modification of ${df.name}")
                    detect = true
                    detect
                  case AST_Unary(UnaryModification(), AST_SymbolRef(_, _, `df`)) =>
                    //println(s"  Detected unary modification of ${df.name}")
                    detect = true
                    detect
                  case _ =>
                    detect
                }
                detect
              }

            }
            if (!assignedInto) {
              val c = varDef.clone()
              c.name = new AST_SymbolConst {
                fillTokens(this, varName)
                init = varName.init
                name = varName.name
                scope = varName.scope
                thedef = varName.thedef
              }
              new AST_Const {
                fillTokens(this, node)
                definitions = js.Array(c)
              }
            } else node
          }
        case _ =>
          node
      }
    }
    AST_Extended(ret, n.types)
  }

  // convert === to ==
  def relations(n: AST_Extended): AST_Extended = {
    val ret = n.top.transformAfter { (node, _) =>
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
    AST_Extended(ret, n.types)
  }

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Node): AST_Node = {

    // walk the tree, check for first reference of each var
    var pairs = Map.empty[SymbolDef, AST_SymbolRef]
    n.walk { node =>
      node match {
        case AST_VarDef(name, value) if value.nonNull.isEmpty =>
          //println(s"AST_VarDef ${name.name}")
          for (df <- name.thedef) {
            assert(df.name == name.name)
            if (df.references.nonEmpty) {
              // find the first reference
              val firstRef = df.references.minBy { ref =>
                assert(ref.thedef.exists(_ == df))
                ref.start.map(_.pos).getOrElse(Int.MaxValue)
              }
              // if the first ref is in the current scope, we might merge it with the declaration
              if (firstRef.scope == name.scope) {
                pairs += df -> firstRef
              }

            }
          }
        case _ =>
      }
      false
    }

    val refs = pairs.values.toSet
    var replaced = Set.empty[SymbolDef]

    //println(s"transform, vars ${pairs.keys.map(_.name).mkString(",")}")

    // walk the tree, check for possible val replacements and perform them
    val changeAssignToVar = n.transformAfter {(node, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      //println(s"node ${nodeClassName(node)} ${ScalaOut.outputNode(node, "")}")
      // we need to descend into assignment definitions, as they may contain other assignments
      node match {
        case AST_SimpleStatement(AST_Assign(sr@AST_SymbolRef(name, scope, thedef), "=", right)) if refs contains sr =>
          //println(s"ss match assign ${nodeClassName(left)} ${ScalaOut.outputNode(left, "")}")
          val stackTail = transformer.stack.takeRight(2).dropRight(1).toSeq
          stackTail match {
            case Seq(_: AST_Block) =>
              val vr = new AST_Var
              val vv = new AST_VarDef
              vr.definitions = js.Array(vv)
              vv.name = new AST_SymbolVar
              vv.name.thedef = thedef
              vv.name.name = name
              vv.name.init = js.Array(right)
              vv.value = right
              vv.name.scope = scope
              //println(s"Replaced ${vv.name.name} AST_SymbolRef with AST_VarDef")
              replaced ++= thedef.nonNull
              vr
            case _ =>
              node
          }
        case _ =>
          node
      }
    }

    //println(s"transform done, replaced ${replaced.map(_.name).mkString(",")}")

    pairs = pairs.filterKeys(replaced.contains)

    // walk the tree, check for possible val replacements and perform them
    changeAssignToVar.transformAfter{ (node, _) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      node match {
        case v: AST_Var =>
          // remove only the original declaration, not the one introduced by us
          // original declaration has no init value
          val af = v.definitions.filterNot { d =>
            d.value.nonNull.isEmpty &&
            d.name.thedef.exists(pairs.contains)
          }
          val vv = v.clone()
          vv.definitions = af
          vv
        case c =>
          c
      }
    }
  }

  def defaultParameterValues(n: AST_Node): AST_Node = {

    def introduceDefaultValues(f: AST_Lambda): AST_Lambda = {

      // the only use of a parameter is in a `x_par || value` form
      def introduceDefaultValue(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
        val parName = par.name
        val references = par.thedef.nonNull.toSeq.flatMap(_.references)

        object IsParDefaultHandling {
          def unapply(arg: AST_Node) = arg match {
            case AST_Binary(symRef@AST_SymbolRefName(`parName`), "||", init: AST_Constant) => Some(symRef, init)
            case _ => None
          }
        }

        if (references.length != 1 ) None
        else {
          var defValue = Option.empty[AST_Node]
          f.walk {
            case IsParDefaultHandling(_, init) =>
              //println(s"Detected def value for $parName")
              defValue = Some(init)
              true
            case _ =>
              defValue.nonEmpty
          }
          defValue.map { init =>
            par.init = js.Array(init)
            // remove the use
            f.transformAfter { (node, _) =>
              node match {
                case IsParDefaultHandling(symRef, _) =>
                  symRef
                case _ =>
                  node
              }
            }
          }
        }
      }

      def processArguments(f: AST_Lambda, args: Seq[AST_SymbolFunarg]): AST_Lambda = args match {
        case Seq() =>
          f
        case head +: tail =>
          introduceDefaultValue(f, head).fold(f) {
            processArguments( _, tail)
          }
      }

      processArguments(f, f.argnames.toSeq.reverse)
    }

    n.transformAfter { (node, _) =>
      node match {
        case f: AST_Defun =>
          introduceDefaultValues(f)
        case m: AST_ConciseMethod =>
          m.value = introduceDefaultValues(m.value)
          m
        case _ =>
          node
      }
    }
  }


  def handleIncrement(n: AST_Node): AST_Node = {

    def substitute(node: AST_Node, expr: AST_SymbolRef, op: String) = {
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
            f.init.exists(_ == n) || f.step.exists(_ == n)
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
        case AST_Unary(op@UnaryModification(), expr: AST_SymbolRef) =>
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
        (ii.body == n || ii.alternative.exists(_ == n)) && nodeLast(ii, parentLevel + 1, transformer)
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
      def lastInSwitch(n: AST_Node): Boolean = {
        transformer.parent().nonNull match {
          case Some(ss: AST_Switch) =>
            ss.body.lastOption.contains(n)
          case _ =>
            false
        }
      }

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
            def processGroup(e: Seq[AST_SwitchBranch], ret: AST_SwitchBranch) = {
              def join(c1: AST_SwitchBranch, c2: AST_SwitchBranch) = {
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
                  join(ret, head)
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
              case _ if !lastInSwitch(s) =>
                // fall through branches - warn
                s.body = s.body ++ js.Array(unsupported("Missing break", s))
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

      case s@AST_SymbolRefDef(symDef) =>
        //val thisScope = findThisScope(Some(symDef.scope))
        //println(s"Sym ${symDef.name} scope ${thisScope.map(_.name.get.name)} type ${types.get(symDef)}")
        types.get(symDef)
      case AST_Dot(cls, name) =>
        for {
          TypeDecl(ClassType(callOn)) <- expressionType(cls)(ctx)
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(c), name)
        } yield {
          //println(s"Infer type of member $c.$name as $r")
          r
        }
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
       // println(s"Infer type of call ${call.name}:$id as ${types.get(id)}")
        types.get(tid)

      case AST_Call(AST_Dot(cls, name), _*) =>
        //println(s"Infer type of member call $name")
        for {
          TypeDecl(ClassType(callOn)) <- expressionType(cls)(ctx)
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(c), name)
        } yield {
          //println(s"  Infer type of member call $c.$name as $r")
          r
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

  case class ClassInfo(members: Set[MemberId] = Set.empty, parents: Map[String, String] = Map.empty) {

    def classContains(cls: String, member: String): Option[String] = {
      val r = if (members contains MemberId(cls, member)) Some(cls)
      else parents.get(cls).flatMap { c =>
        //println(s"  parent $c")
        classContains(c, member)
      }
      //println(s"Check $cls contains $member: $r")
      r
    }

    // list parents, the first in the list is the hierarchy root (no more parents), the last is the class itself
    def listParents(cls: String): Seq[String] = {
      def listParentsRecurse(cls: String, ret: Seq[String]): Seq[String] = {
        val p = parents.get(cls)
        p match {
          case Some(pp) => listParentsRecurse(pp, pp +: ret)
          case None => ret
        }
      }

      listParentsRecurse(cls, Seq(cls))
    }

    def mostDerived(c1: String, c2: String): Option[String] = {
      //println(s"  Parents of $c1: ${listParents(c1)}")
      //println(s"  Parents of $c2: ${listParents(c2)}")
      // check if one is parent of the other
      if (listParents(c1) contains c2) Some(c1)
      else if (listParents(c2) contains c1) Some(c2)
      else None
    }

    def commonBase(c1: String, c2: String): Option[String] = {
      val p1 = listParents(c1)
      val p2 = listParents(c2)
      (p1 zip p2).takeWhile(p => p._1 == p._2).lastOption.map(_._1)
    }
  }

  def listPrototypeMemberNames(cls: AST_DefClass): Set[String] = {
    var existingMembers = Set.empty[String]
    cls.walk {
      case AST_ConciseMethod(AST_SymbolName(p), _) =>
        existingMembers += p
        true
      case AST_ObjectKeyVal(p, _) =>
        existingMembers += p
        true
      case _ =>
        false
    }
    existingMembers
  }

  def listClassMembers(node: AST_Node) = {
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

        val ids = (members ++ varMembers).map(MemberId(clsName, _))

        listMembers = listMembers.copy(members = listMembers.members ++ ids)
        false
      case _ =>
        false
    }
    listMembers
  }

  def inferTypes(n: AST_Extended): AST_Extended = {
    var inferred = SymbolTypes()
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    val classes = classListHarmony(n)
    //println("Classes:\n" + classes)

    val classInfo = listClassMembers(n.top)

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes) // note: ctx.allTypes is mutable

    import ctx.classOps

    def typeFromOperation(op: String, n: AST_Node) = {
      op match {
        case IsComparison() =>
          // comparison - most like the type we are comparing to
          expressionType(n)(ctx)
        case IsArithmetic() =>
          // arithmetics - must be a number,
          // hint: most likely the same type as we are operating with
          Some(TypeInfo.both(number))
        case _ =>
          None
      }
    }

    type TypeInferenceKind = (Option[TypeInfo], Option[TypeInfo]) => Option[TypeInfo]

    def target(t1: Option[TypeInfo], t2: Option[TypeInfo]): Option[TypeInfo] = {
      typeUnionOption(t1, t2)
    }
    def source(t1: Option[TypeInfo], t2: Option[TypeInfo]): Option[TypeInfo] = {
      typeIntersectOption(t1, t2)
    }

    def addInferredType(tid: Option[SymbolMapId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target) = {
      val symType = kind(allTypes.get(tid), tpe)
      //println(s"  Combined $symType = ${allTypes.get(tid)} * $tpe")
      for (tp <- symType) {
        //println(s"  Add type $tid: $tp")
        if (tp.nonEmpty) {
          inferred += tid -> tp
          allTypes.t += tid -> tp
        }
        //println(s"All types ${allTypes.t.types}")
        //println(s"inferred ${inferred.types}")
      }
    }

    def addInferredMemberType(idAccess: Option[MemberId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target) = {

      val id = idAccess.flatMap { i =>
        classInfo.classContains(i.cls, i.name).map { containedIn =>
          i.copy(cls = containedIn)
        }
      }

      val symType = kind(inferred.getMember(id), tpe)
      for (tp <- symType) {
        //println(s"Add member type $idAccess - $id: $tp")
        //println("  " + classInfo)
        if (tp.nonEmpty) {
          inferred = inferred addMember id -> tp
          allTypes.t = allTypes addMember id -> tp
        }
      }
    }

    // list all functions so that we can look-up them from call sites
    var functions = Map.empty[AST_SymbolDeclaration, AST_Defun]
    n.top.walk {
      case defun@AST_Defun(Defined(name),_,_) =>
        functions += name -> defun
        false
      case _ =>
        false
    }

    //println(functions.map(f => f._1.name))


    def inferParsOrArgs(pars: js.Array[AST_SymbolFunarg], args: Seq[AST_Node]) = {
      for {
        (Some(par), arg) <- pars.map(_.thedef.nonNull) zip args
      } {
        val tp = expressionType(arg)(ctx)
        //println(s"Infer par ${par.name} as $tp")
        addInferredType(par, tp)

        arg match {
          case AST_SymbolRefDef(a) => // TODO: SymbolInfo
            val tp = allTypes.get(par)
            //println(s"Infer arg ${a.name} as $tp")
            addInferredType(a, tp, source)
          case _ =>
        }
      }
    }

    def inferFunction(args: Seq[AST_Node]) = {
      val pars = args.map(expressionType(_)(ctx))
      FunctionType(AnyType, pars.flatMap(_.map(_.declType)).toIndexedSeq)
    }

    def inferFunctionReturn(value: AST_Node, r: TypeInfo) = {
      r.declType match {
        case fType: FunctionType =>
          walkLastNode(value) {
            // find any direct returns, when returning a function, infer argument symbol types
            case AST_Lambda(args, body) =>
              //println(s"fun ${args.map(_.name).mkString(",")} -- ${fType.args}")
              //println(s"  $allTypes")

              for {
                (a, tp) <- args zip fType.args
                //_ = println(s"${a.thedef.nonNull.map(_.name)} $tp")
                sym <- a.thedef.nonNull
              } {
                val sid = id(sym)
                if (n.types.get(sid).isEmpty) {
                  //println(s"  Infer arg ${a.name} as $tp")
                  addInferredType(sid, Some(TypeInfo.source(tp)))
                }
              }

              true
            case _ =>
              false

          }
        case _ =>
      }
    }



    def scanFunctionReturns(node: AST_Lambda) = {
      var allReturns = Option.empty[TypeInfo]
      node.walk {
        // include any sub-scopes, but not local functions
        case innerFunc: AST_Lambda if innerFunc != node =>
          true
        case AST_Return(Defined(value)) =>
          //println(s"  return expression ${nodeClassName(value)}")
          val tpe = expressionType(value)(ctx)
          //println(s"  Return type $tpe: expr ${ScalaOut.outputNode(value)}")
          allReturns = typeUnionOption(allReturns, tpe)
          false
        case _ =>
          false
      }
      allReturns
    }

    case class SymbolAccessInfo(symbol: Option[SymbolDef] = None, dot: Option[MemberId] = None) {
      def unknownType(types: SymbolTypes): Boolean = {
        symbol.fold{
          dot.fold(false) { d =>
            val p = findInParents(d.cls, d.name)(ctx)
            //println(s"Check $d => $p = ${n.types.getMember(dot)}")
            n.types.getMember(p.map(pp => d.copy(cls = pp))).isEmpty
          }
        } { s =>
          //println(s"Check $s => ${n.types.get(s)}")
          n.types.get(s).isEmpty
        }
      }

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        for (s <- symbol) {
          addInferredType(s, tpe, kind)
        }
        for (d <- dot) {
          addInferredMemberType(dot, tpe, kind)
        }
      }

      override def toString = {
        symbol.fold(dot.fold("None")(_.toString))(_.name)
      }
    }

    object SymbolInfo {
      def unapply(arg: AST_Node) = arg match {
        case AST_SymbolRefDef(symDef) =>
          Some(SymbolAccessInfo(symbol = Some(symDef)))
        case AST_Dot(expr, name) =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          Some(SymbolAccessInfo(dot = clsId))
        case AST_Call(AST_Dot(expr, name), _*) =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          Some(SymbolAccessInfo(dot = clsId))
        case _ =>
          None
      }
    }

    def inferConstructorCall(args: Seq[AST_Node], className: String) = {
      //println(s"Infer arg types for class $className")
      for (c <- classes.get(className)) {
        {
          val value = classInlineBody(c)
          //println(s"  Constructor inline pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)
        }

        for (AST_ConciseMethod(_, value: AST_Accessor) <- findConstructor(c)) {
          //println(s"  Constructor pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)
        }
      }
    }

    object KnownType {
      def unapply(arg: AST_Node)(implicit ctx: ExpressionTypeContext): Option[TypeInfo] = {
        val tpe = expressionType(arg)(ctx)
        //println(s"  check type of ${ScalaOut.outputNode(arg)} as $tpe")
        tpe
      }
    }

    n.top.walkWithDescend { (node, descend, walker) =>
      //println(s"${nodeClassName(node)}")
      descend(node, walker)

      node match {
        case AST_VarDef(AST_Symbol(_, _, Defined(symDef)), Defined(src)) =>
          if (n.types.get(symDef).isEmpty) {
            //println(s"vardef ${symDef.name} ${nodeClassName(src)} tpe $tpe")
            val tpe = expressionType(src)(ctx)
            addInferredType(symDef, tpe)
            // if this is an inline constructor member, infer also the member type from it
            for {
              cls <- findThisClass(Some(symDef.scope))
              AST_SymbolName(clsName) <- cls.name
              fun <- findThisFunction(Some(symDef.scope))
              body <- findInlineBody(cls)
              if body.value == fun
            } {
              //println(s"vardef ${symDef.name} ${nodeClassName(src)} tpe $tpe")
              //println(s"  inline body cls $clsName")
              addInferredMemberType(Some(MemberId(clsName, symDef.name)), tpe)
            }
          }

        case AST_SymbolFunarg(Defined(symDef), _, Defined(JsArray(init))) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(init)(ctx)
            addInferredType(symDef, tpe)
          }

        case AST_Assign(left, _, right) =>
          val leftT = expressionType(left)(ctx)
          val rightT = expressionType(right)(ctx)
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (SymbolInfo(symInfo) <- Some(left)) {
              //println(s"Infer assign: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT)
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              //println(s"Infer reverse assign: $leftT = $symInfo")
              symInfo.addSymbolInferredType(leftT, source)
            }
          }

        case AST_Binary(SymbolInfo(symLeft), IsArithmetic(), SymbolInfo(symRight))
          if symLeft.unknownType(n.types) && symRight.unknownType(n.types) =>
          //println(s"Infer arithmetic: both unknown $symLeft $symRight")
          val numType = Some(TypeInfo.both(number))
          symLeft.addSymbolInferredType(numType)
          symRight.addSymbolInferredType(numType)

        case AST_Binary(SymbolInfo(symInfo), op, expr) if symInfo.unknownType(n.types) =>
          //println(s"Infer binary: left unknown $symInfo")
          val tpe = typeFromOperation(op, expr)
          symInfo.addSymbolInferredType(tpe)

        case AST_Binary(expr, op, SymbolInfo(symInfo)) if symInfo.unknownType(n.types) =>
          //println(s"Infer binary: right unknown $symInfo")
          val tpe = typeFromOperation(op, expr)
          symInfo.addSymbolInferredType(tpe)

        case fun@AST_Defun(Defined(symDef), _, _) =>
          val allReturns = scanFunctionReturns(fun)
          //println(s"${symDef.name} returns $allReturns")
          addInferredType(symDef.thedef.get, allReturns)

        // TODO: derive getters and setters as well
        case AST_ConciseMethod(AST_SymbolName(sym), fun: AST_Lambda) =>
          val tpe = scanFunctionReturns(fun)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          for (AST_DefClass(Defined(AST_SymbolName(cls)), _, _) <- scope) {
            //println(s"Infer return type for method $cls.$sym as $tpe")
            val classId = MemberId(cls, sym)
            addInferredMemberType(Some(classId), tpe)
          }

        case AST_Call(AST_SymbolRefDef(call), args@_*) =>
          //println(s"Call ${call.name}")
          call.orig.headOption match {
            case Some(clazz: AST_SymbolDefClass) => // constructor call in the new Class(x)
              inferConstructorCall(args, clazz.name)

            case Some(defunSym: AST_SymbolDefun) => // normal function call
              //println(s"Infer arg types for ${defunSym.name}")
              functions.get(defunSym) match {
                case Some(AST_Defun(_, pars, _)) =>
                  // now match arguments to parameters
                  inferParsOrArgs(pars, args)
                case _ =>
              }
            case Some(varSym: AST_SymbolVar) =>
              val tpe = inferFunction(args)
              //println(s"Infer arg types for a var call ${varSym.name} as $tpe")
              varSym.thedef.foreach {
                addInferredType(_, Some(TypeInfo.target(tpe)))
              }
              // TODO: reverse inference
            case _ =>
          }
        case AST_Call(s: AST_Super, args@_*) =>
          for (sup <- findSuperClass(s.scope.nonNull)(ctx)) {
            //println(s"Super call of $sup")
            inferConstructorCall(args, sup)
          }

        case AST_Call(AST_Dot(expr, call), args@_*) =>

          //println(s"Dot call $call")
          // infer types for class member calls
          for {
            TypeDecl(ClassType(callOn)) <- expressionType(expr)(ctx)
            clazz <- classes.get(callOn)
            c <- includeParents(clazz, Seq(clazz))(ctx) // infer for all overrides
          } {
            findMethod(c, call).fold {
              val tpe = inferFunction(args)

              //println(s"Infer arg types for a var member call ${c.name.get.name} as $tpe")
              val memberId = c.name.nonNull.map(n => MemberId(n.name, call))
              addInferredMemberType(memberId, Some(TypeInfo.target(tpe))) // target or source?

              for {
                AST_ObjectKeyVal(p, a) <- findProperty(c, call)
                r <- allTypes.getMember(memberId)
              } {
                //println(s"Infer $p $r ${nodeClassName(a)}")
                inferFunctionReturn(a, r)
              }

              // if there are any return statements, we can infer types for them
              // beware of IIFE
              // TODO: reverse inference
            } { m =>
              inferParsOrArgs(m.value.argnames, args)
            }
          }

        case _ =>
      }
      true
    }
    // TODO: protect JSDoc explicit types
    n.copy(types = n.types ++ inferred)
  }

  def inferTypesMultipass(n: AST_Extended): AST_Extended = {

    def inferTypesStep(n: AST_Extended, maxDepth: Int = 50): AST_Extended = {
      //println(s"Type inference: ${n.types}")
      val r = inferTypes(n)
      //println(s"Type inference done: ${r.types}")
      if (r.types != n.types && maxDepth > 0) inferTypesStep(r, maxDepth - 1)
      else r
    }

    inferTypesStep(n)
  }


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
        case AST_Var(AST_VarDef(AST_SymbolDef(sym), Defined(AST_BlockStatement(DefineAndReturnClass(defClass, r))))) if sym.name == r.name =>
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
            case s@AST_SimpleStatement(AST_Call(AST_Dot(AST_SymbolRef("Object", _, _), "assign"), ts@AST_SymbolRefDef(sym), x: AST_Object)) =>
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

    val transforms = Seq(
      onTopNode(handleIncrement),
      onTopNode(varInitialization),
      readJSDoc _,
      onTopNode(iife), // removes also trailing return within the IIFE construct
      onTopNode(removeDoubleScope) // after iife (often introduced by it)
    ) ++ TransformClasses.transforms ++ Seq(
      onTopNode(defaultParameterValues),
      onTopNode(varInitialization), // already done, but another pass is needed after TransformClasses
      objectAssign _,
      onTopNode(removeVarClassScope),
      inferTypesMultipass _,
      onTopNode(removeTrailingBreak), // before removeTrailingReturn, return may be used to terminate cases
      onTopNode(removeTrailingReturn), // after inferTypes (returns are needed for inferTypes)
      detectVals _,
      relations _
    )

    transforms.foldLeft(AST_Extended(n, SymbolTypes())) { (t,op) =>
      t.top.figure_out_scope()
      op(t)
    }
  }
}
