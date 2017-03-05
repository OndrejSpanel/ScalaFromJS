package com.github.opengrabeso

import com.github.opengrabeso.JsUtils._
import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt._
import com.github.opengrabeso.UglifyExt.Import._

import scala.collection.mutable
import scala.scalajs.js
import js.JSConverters._

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

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Extended): AST_Extended = {

    // walk the tree, check for first reference of each var
    var pairs = Map.empty[SymbolDef, AST_SymbolRef]
    n.top.walk { node =>
      node match {
        case AST_VarDef(name, value) if value.nonNull.isEmpty =>
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

              val operation = new AST_SimpleStatement {
                fillTokens(this, node)
                body = substitute(node, expr, op)
              }
              val value = new AST_SimpleStatement {
                fillTokens(this, node)
                body = expr.clone()
              }
              node match {
                case _: AST_UnaryPrefix =>
                  this.body = js.Array(operation, value)
                case _ /*: AST_UnaryPostfix*/ =>
                  this.body = js.Array(value, operation)
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
        transformer.parent(parentLevel) match {
          case fun: AST_Lambda =>
            fun.body.last == n
          case block: AST_Block  =>
            block.body.last == n && parentLevel < transformer.stack.length - 2 && nodeLast(block, parentLevel + 1)
          case ii: AST_If =>
            (ii.body == n || ii.alternative.exists(_ == n)) && nodeLast(ii, parentLevel + 1)
          case _ =>
            false
        }
      }

      node match {
        case ret: AST_Return if nodeLast(ret, 0) =>
          // check if last in a function body
          ret.value.nonNull.getOrElse {
            new AST_EmptyStatement {
              fillTokens(this, ret)
            }
          }
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

  def expressionType(n: AST_Node)(types: SymbolTypes): Option[TypeDesc] = {
    //println(nodeClassName(n) + ": " + ScalaOut.outputNode(n, ""))
    n match {
      case AST_SymbolRefDef(symDef) =>
        types.get(symDef)
      case _: AST_Number =>
        Some(SymbolTypes.number)
      case _: AST_String =>
        Some(SymbolTypes.string)
      case _: AST_Boolean =>
        Some(SymbolTypes.boolean)
      case AST_Binary(left, op, right) =>
        // sometimes operation is enough to guess an expression type
        // result of any arithmetic op is a number
        op match {
          case IsArithmetic() => Some(SymbolTypes.number)
          case IsComparison() => Some(SymbolTypes.boolean)
          case "+" =>
            val typeLeft = expressionType(left)(types)
            val typeRight = expressionType(right)(types)
            // string + anything is a string
            if (typeLeft == typeRight) typeLeft
            else if (typeLeft.contains(SymbolTypes.string) || typeRight.contains(SymbolTypes.string)) Some(SymbolTypes.string)
            else None
          case IsBoolean() =>
            // boolean with the same type is the same type
            val typeLeft = expressionType(left)(types)
            val typeRight = expressionType(right)(types)
            if (typeLeft == typeRight) typeLeft
            else None
          case _ =>
            None
        }
      case AST_New(AST_SymbolRefDef(call), _*) =>
        Some(call.name)
      case AST_Call(AST_SymbolRefDef(call), _*) =>
        types.get(call)
      case seq: AST_Seq =>
        expressionType(seq.cdr)(types)
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
      // classes expected to be top-level
      case _ : AST_Toplevel =>
        false
      case _ =>
        true
    }
    classes
  }


  def inferTypes(n: AST_Extended): AST_Extended = {
    var inferred = SymbolTypes()
    var allTypes = n.types // all known types including the inferred ones
    // TODO: consider multipass, can help with forward references

    val classes = classListHarmony(n)
    //println(classes)


    def typeFromOperation(op: String, n: AST_Node) = {
      op match {
        case IsComparison() =>
          // comparison - most like the type we are comparing to
          expressionType(n)(allTypes)
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
        //println(s"Add type ${symDef.name}: $tpe")
        inferred += symDef -> tp
        allTypes += symDef -> tp
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


    def inferParsOrArgs(pars: js.Array[AST_SymbolFunarg], args: Seq[AST_Node]) = {
      for {
        (Some(par), arg) <- pars.map(_.thedef.nonNull) zip args
      // only when the type is not provided explicitly
      } {
        if (n.types.get(par).isEmpty) {
          val tp = expressionType(arg)(allTypes)
          //println(s"Infer ${par.name} as $tp")
          addInferredType(par, tp)
        }
        arg match {
          case AST_SymbolRefDef(a) if n.types.get(a).isEmpty =>
            val tp = allTypes.get(par)
            //println(s"Infer ${a.name} as $tp")
            addInferredType(a, tp)
          case _ =>
        }
      }
    }



    n.top.walkWithDescend { (node, descend, walker) =>
      descend(node, walker)
      node match {
        case AST_VarDef(AST_Symbol(_, _, Defined(symDef)),Defined(src)) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(src)(allTypes)
            addInferredType(symDef, tpe)
          }

        case AST_Assign(AST_SymbolRefDef(symDef), _, src) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(src)(allTypes)
            addInferredType(symDef, tpe)
          }

        case AST_Binary(AST_SymbolRefDef(symLeft), IsArithmetic(), AST_SymbolRefDef(symRight))
          if n.types.get(symLeft).isEmpty && n.types.get(symRight).isEmpty =>
          val numType = Some(SymbolTypes.number)
          addInferredType(symLeft, numType)
          addInferredType(symRight, numType)

        case AST_Binary(AST_SymbolRefDef(symDef), op, expr) if n.types.get(symDef).isEmpty =>
          val tpe = typeFromOperation(op, expr)
          addInferredType(symDef, tpe)

        case AST_Binary(expr, op, AST_SymbolRefDef(symDef)) if n.types.get(symDef).isEmpty =>
          val tpe = typeFromOperation(op, expr)
          addInferredType(symDef, tpe)

        case AST_Defun(Defined(symDef), _, _) =>

          var allReturns = Option.empty[TypeDesc]
          //println(s"Defun ${symDef.name}")
          //println("  " + allTypes.toString)
          node.walk {
            // include any sub-scopes, but not local functions
            case innerFunc: AST_Lambda if innerFunc != node =>
              true
            case AST_Return(Defined(value)) =>
              val tp = expressionType(value)(allTypes)
              //println(s"Return type $tp: expr ${ScalaOut.outputNode(value, "")}")
              allReturns = SymbolTypes.typeUnionOption(allReturns, tp)
              false
            case _ =>
              false
          }
          addInferredType(symDef.thedef.get, allReturns)

        case AST_Call(AST_SymbolRefDef(call), args@_*) =>

          // get the AST_Defun node to get the arg symbols from it
          call.orig.headOption match {
            case Some(clazz: AST_SymbolDefClass) =>
              //println(s"Infer arg types for class ${clazz.name}")

              for {
                c <- classes.get(clazz.name)
                AST_ConciseMethod(_, value: AST_Accessor) <- findConstructor(c)
              } {
                //println(s"  Constructor args ${value.argnames}")
                inferParsOrArgs(value.argnames, args)
              }

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
        case AST_Call(AST_Dot(expr,call), args@_*) =>


          def includeParents(clazz: AST_DefClass, ret: Seq[AST_DefClass]): Seq[AST_DefClass] = {
            clazz.`extends`.nonNull match {
              case Some(cls: AST_SymbolRef) =>
                val c = classes.get(cls.name)
                c.fold(ret)(parent => includeParents(parent, parent +: ret))
              case _ => ret
            }
          }
          //println(s"Call $call")
          // infer types for class member calls
          for {
            callOn <- expressionType(expr)(allTypes)
            clazz <- classes.get(callOn) // TODO: search through bases as well
            c <- includeParents(clazz, Seq(clazz))
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

  val isConstructorProperty: PartialFunction[AST_ObjectProperty, AST_ConciseMethod] = {
    case m: AST_ConciseMethod if m.key.name == "constructor" =>
      m
  }

  def findConstructor(c: AST_DefClass): Option[AST_ConciseMethod] = {
    c.properties.collect(isConstructorProperty).headOption
  }

  def findMethod(c: AST_DefClass, name: String): Option[AST_ConciseMethod] = {
    c.properties.collect {
      case m: AST_ConciseMethod if m.key.name == name => m
    }.headOption
  }

  def funcScope(n: AST_Extended): AST_Extended = {
    //println(nodeTreeToString(n.top))

    object DefineAndReturnClass {
      def unapply(arg: Seq[AST_Statement]) = arg match {
        case Seq(defClass@AST_DefClass(Defined(c), _, _), AST_Return(Defined(r : AST_SymbolRef))) if c.thedef == r.thedef =>
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
                  println(s"${p.key}")
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

    val transforms: Seq[(AST_Extended) => AST_Extended] = Seq(
      handleIncrement,
      varInitialization,
      readJSDoc,
      //objectAssign,
      TransformClasses.apply,
      funcScope,
      removeTrailingReturn,
      inferTypes,
      detectVals
    )

    transforms.foldLeft(AST_Extended(n, SymbolTypes())) { (t,op) =>
      t.top.figure_out_scope()
      op(t)
    }
  }
}
