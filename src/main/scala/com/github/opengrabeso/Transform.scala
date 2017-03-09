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

  type TypeDesc = SymbolTypes.TypeDesc

  object AST_Extended {
    def noTypes = SymbolTypes()
    def apply(top: AST_Node, types: SymbolTypes): AST_Extended = new AST_Extended(top.asInstanceOf[AST_Toplevel], types)
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
            // TODO: infer type
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
        case bin@AST_Binary(left, "===", right) =>
          bin.operator = "=="
          node
        case bin@AST_Binary(left, "!==", right) =>
          bin.operator = "!="
          node
        case _ =>
          node
      }
    }
    AST_Extended(ret, n.types)
  }

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Extended): AST_Extended = {

    // walk the tree, check for first reference of each var
    var pairs = Map.empty[SymbolDef, AST_SymbolRef]
    n.top.walk { node =>
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
    val changeAssignToVar = n.top.transformAfter {(node, transformer) =>
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
    val ret = changeAssignToVar.transformAfter{ (node, _) =>
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


    AST_Extended(ret, n.types)
  }

  def handleIncrement(n: AST_Extended): AST_Extended = {

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

//    implicit class InitNode(node: AST_Node) {
//      def init(f: AST_Node => AST_Node) = f(node)
//    }

    // walk the tree, check for increment / decrement
    val t = n.top.transformAfter { (node, transformer) =>
      def nodeResultDiscarded(n: AST_Node, parentLevel: Int): Boolean = {
        transformer.parent(parentLevel) match {
          case _: AST_SimpleStatement =>
            true
          case f: AST_For =>
            // can be substituted inside of for unless used as a condition
            f.init.exists(_ == n) || f.step.exists(_ == n)
          case s: AST_Seq  =>
            if (s.cdr !=n) true
            else if (parentLevel < transformer.stack.length - 2) {
              // even last item of seq can be substituted when the seq result is discarded
              nodeResultDiscarded(s, parentLevel+1)
            } else false
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


    AST_Extended(t, n.types)
  }



  def removeTrailingReturn(n: AST_Extended): AST_Extended = {
    val t = n.top.transformAfter { (node, transformer) =>

      def nodeLast(n: AST_Node, parentLevel: Int): Boolean = {
        //println(s"${nodeClassName(n)}:$parentLevel:${nodeClassName(transformer.parent(parentLevel))}")
        transformer.parent(parentLevel) match {
          case ss: AST_SimpleStatement =>
            ss.body == n
          case fun: AST_Lambda =>
            fun.body.lastOption contains n
          case block: AST_Block  =>
            (block.body.lastOption contains n) && parentLevel < transformer.stack.length - 2 && nodeLast(block, parentLevel + 1)
          case ii: AST_If =>
            (ii.body == n || ii.alternative.exists(_ == n)) && nodeLast(ii, parentLevel + 1)
          case _ =>
            false
        }
      }

      node match {
        case ret: AST_Return if nodeLast(ret, 0) =>
          // check if last in a function body
          //println("Remove trailing return")
          ret.value.nonNull.fold[AST_Statement] {
            new AST_EmptyStatement {
              fillTokens(this, ret)
            }
          }{ v =>
              new AST_SimpleStatement {
                fillTokens(this, ret)
                body = v

              }
          }
        case _ =>
          node
      }
    }
    AST_Extended(t, n.types)
  }

  def removeTrailingBreak(n: AST_Extended): AST_Extended = {

    val t = n.top.transformAfter { (node, transformer) =>
      def lastInSwitch(n: AST_Node): Boolean = {
        transformer.parent() match {
          case ss: AST_Switch =>
            ss.body.lastOption.contains(n)
          case _ =>
            false
        }
      }

      node match {
        case sw: AST_Switch =>
          val newBody = new mutable.ArrayBuffer[AST_Statement]
          var emptyBefore = Seq.empty[AST_SwitchBranch] // accumulated list of empty conditions
          for ((s: AST_SwitchBranch) <- sw._body.asInstanceOf[js.Array[AST_SwitchBranch]]) {

            if (s.body.nonEmpty) { // flush emptyBefore
              def processEmptyPrefix(e: Seq[AST_Node]): Unit = {
                e match {
                  case head +: tail =>
                    (s, head) match {
                      case (c1: AST_Case, c2: AST_Case) =>
                        c1.expression = new AST_Binary {
                          fillTokens(this, c1.expression)
                          left = c1.expression
                          operator = "|"
                          right = c2.expression
                        }
                      case (c: AST_Default, _) =>
                        // TODO: emit a default ...
                      case (_, c: AST_Default) =>
                        // already a default, no need to append
                    }
                    processEmptyPrefix(tail)
                  case _ =>
                }

              }
              processEmptyPrefix(emptyBefore)
              emptyBefore = Seq.empty
            }
            s.body.lastOption match {
              case Some(_: AST_Break) =>
                s.body = s.body.dropRight(1)
                newBody append s
              case Some(_: AST_Throw) =>
                newBody append s
              case Some(_: AST_Return) =>
                newBody append s
              case None =>
                emptyBefore = s +: emptyBefore
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
    AST_Extended(t, n.types)
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
                // TODO: only comment blocks starting with JSDoc marker
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
                      declBuffer append td -> tpe
                    }
                  case JSDocReturn(tpe) =>
                    for {
                      s <- f.name.nonNull
                      td <- s.thedef.nonNull
                    } {
                      declBuffer append td -> tpe
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
  case class ExpressionTypeContext(types: Ref[SymbolTypes], classInfo: ClassInfo, classes: Map[TypeDesc, AST_DefClass])


  def expressionType(n: AST_Node)(ctx: ExpressionTypeContext): Option[TypeDesc] = {
    //println(nodeClassName(n) + ": " + ScalaOut.outputNode(n, ""))
    import ctx._
    n match {
      case s: AST_Super =>
        val sup = findSuperClass(s.scope.nonNull)(ctx)
        //println(s"super scope $sup")
        sup

      case t: AST_This =>
        val thisScope = findThisScope(t.scope.nonNull)
        //println(s"this scope ${t.scope.map(_.nesting)}")
        val cls = thisScope.flatMap(_.name.nonNull).map(_.name)
        //println(s"this def scope $cls")
        cls

      case s@AST_SymbolRefDef(symDef) =>
        //val thisScope = findThisScope(Some(symDef.scope))
        //println(s"Sym ${symDef.name} scope ${thisScope.map(_.name.get.name)} type ${types.get(symDef)}")
        types.get(symDef)
      case AST_Dot(cls, name) =>
        for {
          callOn <- expressionType(cls)(ctx)
          c <- findInParents(callOn, name)(ctx)
          r <- types.getMember(Some(c), name)
        } yield {
          //println(s"Infer type of member $c.$name as $r")
          r
        }
      case _: AST_Number =>
        Some(SymbolTypes.number)
      case _: AST_String =>
        Some(SymbolTypes.string)
      case _: AST_Boolean =>
        Some(SymbolTypes.boolean)
      case tern: AST_Conditional =>
        val t1 = expressionType(tern.consequent)(ctx)
        val t2 = expressionType(tern.consequent)(ctx)
        SymbolTypes.typeUnionOption(t1, t2)
      case AST_Binary(left, op, right) =>
        // sometimes operation is enough to guess an expression type
        // result of any arithmetic op is a number
        op match {
          case IsArithmetic() => Some(SymbolTypes.number)
          case IsComparison() => Some(SymbolTypes.boolean)
          case "+" =>
            val typeLeft = expressionType(left)(ctx)
            val typeRight = expressionType(right)(ctx)
            // string + anything is a string
            if (typeLeft == typeRight) typeLeft
            else if (typeLeft.contains(SymbolTypes.string) || typeRight.contains(SymbolTypes.string)) Some(SymbolTypes.string)
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
        Some(call.name)
      case AST_Call(AST_SymbolRefDef(call), _*) =>
        val id = SymbolTypes.id(call)
       // println(s"Infer type of call ${call.name}:$id as ${types.get(id)}")
        types.get(id)

      case AST_Call(AST_Dot(cls, name), _*) =>
        //println(s"Infer type of member call $name")
        for {
          callOn <- expressionType(cls)(ctx)
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
    var classes = Map.empty[TypeDesc, AST_DefClass]
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

  case class ClassInfo(members: Set[SymbolTypes.MemberId] = Set.empty, parents: Map[TypeDesc, TypeDesc] = Map.empty) {
    def classContains(cls: String, member: String): Option[String] = {
      val r = if (members contains SymbolTypes.MemberId(cls, member)) Some(cls)
      else parents.get(cls).flatMap{c =>
        //println(s"  parent $c")
        classContains(c, member)
      }
      //println(s"Check $cls contains $member: $r")
      r
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
        val varMembers = for (VarName(member) <- TransformClasses.classInlineBody(cls).body) yield member

        val ids = (members ++ varMembers).map(SymbolTypes.MemberId(clsName, _))

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
    // TODO: consider multipass, can help with forward references

    val classes = classListHarmony(n)
    //println("Classes:\n" + classes)

    val classInfo = listClassMembers(n.top)

    val ctx = ExpressionTypeContext(allTypes, classInfo, classes) // note: ctx.allTypes is mutable

    def typeFromOperation(op: String, n: AST_Node) = {
      op match {
        case IsComparison() =>
          // comparison - most like the type we are comparing to
          expressionType(n)(ctx)
        case IsArithmetic() =>
          // arithmetics - must be a number,
          // hint: most likely the same type as we are operating with
          Some(SymbolTypes.number)
        case _ =>
          None
      }
    }

    def addInferredType(symDef: SymbolDef, tpe: Option[TypeDesc]) = {
      val symType = SymbolTypes.typeUnionOption(tpe, inferred.get(symDef))
      for (tp <- symType) {
        val id = SymbolTypes.id(symDef)
        //println(s"Add type ${symDef.name}: $tpe id = $id")
        inferred += id -> tp
        allTypes.t += id -> tp
        //println(s"All types ${allTypes.t.types}")
      }
    }

    def addInferredMemberType(idAccess: Option[SymbolTypes.MemberId], tpe: Option[TypeDesc]) = {

      val id = idAccess.flatMap { i =>
        classInfo.classContains(i.cls, i.name).map { containedIn =>
          i.copy(cls = containedIn)
        }
      }

      val symType = SymbolTypes.typeUnionOption(tpe, inferred.getMember(id))
      for (tp <- symType) {
        //println(s"Add member type $idAccess - $id: $tp")
        //println("  " + classInfo)
        inferred = inferred addMember id -> tp
        allTypes.t = allTypes addMember id -> tp
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
      // only when the type is not provided explicitly
      } {
        if (n.types.get(par).isEmpty) {
          val tp = expressionType(arg)(ctx)
          //println(s"Infer par ${par.name} as $tp")
          addInferredType(par, tp)
        }
        arg match {
          case AST_SymbolRefDef(a) if n.types.get(a).isEmpty =>
            val tp = allTypes.get(par)
            //println(s"Infer arg ${a.name} as $tp")
            addInferredType(a, tp)
          case _ =>
        }
      }
    }

    def scanFunctionReturns(node: AST_Lambda) = {
      var allReturns = Option.empty[TypeDesc]
      node.walk {
        // include any sub-scopes, but not local functions
        case innerFunc: AST_Lambda if innerFunc != node =>
          true
        case AST_Return(Defined(value)) =>
          //println(s"  return expression ${nodeClassName(value)}")
          val tpe = expressionType(value)(ctx)
          //println(s"  Return type $tpe: expr ${ScalaOut.outputNode(value, "")}")
          allReturns = SymbolTypes.typeUnionOption(allReturns, tpe)
          false
        case _ =>
          false
      }
      allReturns
    }

    case class SymbolAccessInfo(symbol: Option[SymbolDef] = None, dot: Option[SymbolTypes.MemberId] = None) {
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

      def addSymbolInferredType(tpe: Option[SymbolTypes.TypeDesc]): Unit = {
        for (s <- symbol) {
          addInferredType(s, tpe)
        }
        for (d <- dot) {
          addInferredMemberType(dot, tpe)
        }
      }

      override def toString = {
        symbol.fold(dot.toString)(_.name)
      }
    }

    object SymbolInfo {
      def unapply(arg: AST_Node) = arg match {
        case AST_SymbolRefDef(symDef) =>
          Some(SymbolAccessInfo(symbol = Some(symDef)))
        case AST_Dot(expr, name) =>
          val clsId = SymbolTypes.memberId(expressionType(expr)(ctx), name)
          Some(SymbolAccessInfo(dot = clsId))
        case AST_Call(AST_Dot(expr, name), _*) =>
          val clsId = SymbolTypes.memberId(expressionType(expr)(ctx), name)
          Some(SymbolAccessInfo(dot = clsId))
        case _ =>
          None
      }
    }

    def inferConstructorCall(args: Seq[AST_Node], className: TypeDesc) = {
      //println(s"Infer arg types for class $className")
      for (c <- classes.get(className)) {
        {
          val value = TransformClasses.classInlineBody(c)
          //println(s"  Constructor inline pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)
        }

        for (AST_ConciseMethod(_, value: AST_Accessor) <- findConstructor(c)) {
          //println(s"  Constructor pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)
        }
      }
    }

    n.top.walkWithDescend { (node, descend, walker) =>
      descend(node, walker)

      node match {
        case AST_VarDef(AST_Symbol(_, _, Defined(symDef)),Defined(src)) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(src)(ctx)
            addInferredType(symDef, tpe)
          }

        case AST_Assign(SymbolInfo(symInfo), _, src) =>
          val tpe = expressionType(src)(ctx)
          //println(s"Infer assign: $symInfo $tpe")
          symInfo.addSymbolInferredType(tpe)

        case AST_Binary(SymbolInfo(symLeft), IsArithmetic(), SymbolInfo(symRight))
          if symLeft.unknownType(n.types) && symRight.unknownType(n.types) =>
          //println(s"Infer arithmetic: both unknown $symLeft $symRight")
          val numType = Some(SymbolTypes.number)
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
          val scope = findThisScope(fun.parent_scope.nonNull)
          for (AST_DefClass(Defined(AST_SymbolName(cls)), _, _) <- scope) {
            //println(s"Infer return type for method $cls.$sym as $tpe")
            val classId = SymbolTypes.MemberId(cls, sym)
            addInferredMemberType(Some(classId), tpe)
          }

        case AST_Call(AST_SymbolRefDef(call), args@_*) =>
          // we may call a function or a class (constructor call in the new Class(x)
          call.orig.headOption match {
            case Some(clazz: AST_SymbolDefClass) =>
              inferConstructorCall(args, clazz.name)

            case Some(defunSym: AST_SymbolDefun) =>
              //println(s"Infer arg types for ${defunSym.name}")

              functions.get(defunSym) match {
                case Some(AST_Defun(_, pars, _)) =>
                  // now match arguments to parameters
                  inferParsOrArgs(pars, args)
                case _ =>
              }

            case _ =>
          }
        case AST_Call(s: AST_Super, args@_*) =>
          for (sup <- findSuperClass(s.scope.nonNull)(ctx)) {
            //println(s"Super call of $sup")
            inferConstructorCall(args, sup)
          }

        case AST_Call(AST_Dot(expr,call), args@_*) =>

          //println(s"Call $call")
          // infer types for class member calls
          for {
            callOn <- expressionType(expr)(ctx)
            clazz <- classes.get(callOn)
            c <- includeParents(clazz, Seq(clazz))(ctx)
            //_ = println(s"${c.name.get.name}")
            AST_ConciseMethod(_, value: AST_Accessor) <- findMethod(c, call)
          } {
            //println(s"  Call args ${value.argnames.map(_.name).mkString(",")}")
            //println(s"  Call pars ${args.map(expressionType(_)(allTypes)).mkString(",")}")
            inferParsOrArgs(value.argnames, args)
          }

        case _ =>
      }
      true
    }
    // do not overwrite explicit types by inferred ones
    n.copy(types = inferred ++ n.types)
  }

  def inferTypesMultipass(n: AST_Extended): AST_Extended = {

    def inferTypesStep(n: AST_Extended, maxDepth: Int = 50): AST_Extended = {
      val r = inferTypes(n)
      //println(s"Type inference done: ${n.types}")
      if (r.types != n.types && maxDepth > 0) inferTypesStep(r, maxDepth - 1)
      else r
    }

    inferTypesStep(n)
  }

  val isConstructorProperty: PartialFunction[AST_ObjectProperty, AST_ConciseMethod] = {
    case m: AST_ConciseMethod if m.key.name == "constructor" =>
      m
  }

  def findConstructor(c: AST_DefClass): Option[AST_ConciseMethod] = {
    c.properties.collectFirst(isConstructorProperty)
  }

  def findMethod(c: AST_DefClass, name: String): Option[AST_ConciseMethod] = {
    c.properties.collect {
      case m: AST_ConciseMethod if m.key.name == name => m
    }.headOption
  }

  def funcScope(n: AST_Extended): AST_Extended = {
    //println(nodeTreeToString(n.top))

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

    // "Immediately-invoked function expression"
    object IIFE {
      def unapply(arg: AST_Node) = arg match {
        case AST_Call(l@AST_Lambda(args1, funcBody), args2@_*) if args1.isEmpty && args2.isEmpty =>
          Some(funcBody)
        case _ => None

      }
    }

    // first try to match a special var form, as transformAfter is depth first
    val classToVar = n.top.transformAfter { (node, _) =>
      node match {
        // var Name = (function () { class Name() { } return Name; } ) ();
        case AST_Var(AST_VarDef(AST_SymbolDef(sym), Defined(IIFE(DefineAndReturnClass(defClass, r))))) if sym.name == r.name =>
          defClass
        case _ =>
          node
      }
    }

    val ret = classToVar.transformAfter { (node, _) =>
      node match {
        case IIFE(DefineAndReturnClass(defClass, _)) =>
          defClass
        case IIFE(funcBody) =>
          new AST_BlockStatement {
            fillTokens(this, node)
            this.body = funcBody.toJSArray
          }
        case _ =>
          node
      }
    }
    AST_Extended(ret, n.types)
  }

  // IIFE removal sometimes leaves two scopes directly in one another
  def removeDoubleScope(n: AST_Extended): AST_Extended = {
    val ret = n.top.transformAfter { (node, _) =>
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
          println("Remove AST_Accessor <- AST_BlockStatement")
          func.body = body.toJSArray
          func
        case func@AST_Lambda(_, body) =>
          //println(nodeClassName(func))
          //println(body.map(nodeClassName))
          func
        case _ =>
          node
      }
    }

    AST_Extended(ret, n.types)
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

  def apply(n: AST_Toplevel): AST_Extended = {

    val transforms = Seq(
      handleIncrement _,
      varInitialization _,
      readJSDoc _
    ) ++ TransformClasses.transforms ++ Seq(
      varInitialization _, // already done, but another pass is needed after TransformClasses
      objectAssign _,
      inferTypesMultipass _,
      removeTrailingBreak _, // before removeTrailingReturn, return may be used to terminate cases
      removeTrailingReturn _, // after inferTypes (returns are needed for inferTypes)
      funcScope _, // after removeTrailingReturn (functions scopes are needed for return removal)
      removeDoubleScope _, // after funcScope (often introduced by it)
      detectVals _,
      relations _
    )

    transforms.foldLeft(AST_Extended(n, SymbolTypes())) { (t,op) =>
      t.top.figure_out_scope()
      op(t)
    }
  }
}
