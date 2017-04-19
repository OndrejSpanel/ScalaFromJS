package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._

import scala.scalajs.js
import js.JSConverters._
import scala.collection.mutable
import scala.language.implicitConversions

object Variables {
  import VariableUtils._

  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Node): AST_Node = {
    // walk the tree, check for possible val replacements and perform them
    val refs = buildReferenceStacks(n)

    n.transformBefore {(node, descend, transformer) =>
      node match {
        case cm: AST_ConciseMethod =>
          if (cm.key.name != inlineBodyName) {
            // no var detection inside of inline class body (its variables are actually class members)
            val n = cm.clone()
            descend(n, transformer)
            n
          } else cm.clone()

        case AST_Var(varDef@AST_VarDef(varName, value)) if value.nonNull.nonEmpty => // var with init - search for a modification
          //println(s"AST_VarDef ${varName.name}")
          varName.thedef.fold(node) { df =>
            assert(df.name == varName.name)
            // check if any reference is assignment target

            object IsDf extends Extractor[Unit] {
              def unapply(arg: AST_Node) = arg match {
                case AST_SymbolRefDef(`df`) => Some(())
                case _ => None
              }
            }
            object IsDfModified extends IsModified(IsDf)

            val assignedInto = refs.walkReferences(df, IsDfModified)(_ => true)
            val n = if (!assignedInto) {
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
            } else {
              node.clone()
            }
            descend(n, transformer) // may contain other scopes with initializations
            n
          }
        case _ =>
          val n = node.clone()
          descend(n, transformer)
          n
      }
    }
  }

  // detect function key values can be declared as concise methods instead
  def detectMethods(n: AST_Node): AST_Node = {
    val refs = buildReferenceStacks(n)

    n.transformBefore {(node, descend, transformer) =>
      node match {
        case obj@AST_Object(props) =>
        //case AST_Definitions(AST_VarDef(AST_SymbolDef(df), Defined(obj@AST_Object(props)))) =>
          // check which members are ever written to - we can convert all others to getters and methods
          transformer.parent(1).nonNull match {
            case Some(AST_Definitions(AST_VarDef(AST_SymbolDef(df), Defined(o: AST_Object)))) =>
              //println(s"Scan object ${df.name} for methods")
              assert(o == obj)


              object IsDf extends Extractor[String] {
                def unapply(arg: AST_Node) = arg match {
                  case AST_SymbolRefDef(`df`) AST_Dot key => Some(key)
                  case _ => None
                }
              }
              object IsDfModified extends IsModified(IsDf)

              var modifiedMembers = Set.empty[String]
              refs.walkReferences(df, IsDfModified) { key =>
                modifiedMembers += key
                false
              }

              //println(s"Detected modified members $modifiedMembers")

              val newProps = props.map {
                case kv@AST_ObjectKeyVal(k, f@AST_Function(args, body)) =>
                  if (modifiedMembers contains k) {
                    //println(s"Modified member $k")
                    kv
                  } else newMethod(k, args, body, f)

                case p => p
              }
              val newObj = obj.clone()
              newObj.properties = newProps.toJSArray
              newObj

            case None =>
              node
            case Some(p) =>
              //println(s"Parent ${nodeClassName(p)}")
              node
          }
        case _ =>
          val n = node.clone()
          descend(n, transformer)
          n
      }
    }
  }

  def convertConstToFunction(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case AST_Const(AST_VarDef(sym, Defined(AST_Function(args, body)))) =>
          new AST_Defun {
            defun =>
            name = new AST_SymbolDefun {
              name = sym.name
              thedef = sym.thedef
              scope = sym.scope
              init = js.Array[AST_Node](defun)
            }
            fillTokens(this, node)
            argnames = args.toJSArray
            this.body = body.toJSArray
          }
        case _ => node
      }
    }
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
                assert(ref.thedef contains df)
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

    object MatchInitWithAssign {
      def unapply(arg: AST_Node) = arg match {
        // sr = xxx
        case AST_SimpleStatement(AST_Assign(sr@AST_SymbolRef(name, scope, thedef), "=", right)) if refs contains sr =>
          Some(sr, name, scope, thedef, right)
        // if (m1 == undefined) m1 = xxx
        case AST_If(
        AST_Binary(sr@AST_SymbolRef(name, scope, thedef), "==" | "===", AST_SymbolRefName("undefined")),
        SingleStatement(AST_Assign(sr2@AST_SymbolRef(_, _, thedef2), "=", right)),
        None
        ) if thedef == thedef2 && (refs contains sr) =>
          Some(sr, name, scope, thedef, right)
        case _ => None
      }
    }

    // walk the tree, check for possible val replacements and perform them
    val changeAssignToVar = n.transformAfter {(node, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      //println(s"node ${nodeClassName(node)} ${ScalaOut.outputNode(node, "")}")
      // we need to descend into assignment definitions, as they may contain other assignments
      node match {
        case MatchInitWithAssign(sr, name, scope, thedef, right) =>
          //println(s"ss match assign ${nodeClassName(left)} ${ScalaOut.outputNode(left, "")}")
          // checking if inside of a block statement, to prevent replacing inside of a compound statement
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
              for (d <- thedef; orig <- d.orig.headOption) {
                fillTokens(vr, orig)
                fillTokens(vv, orig)
                fillTokens(vv.name, orig)
              }
              //println(s"Replaced ${vv.name.name} AST_SymbolRef with AST_VarDef, value ${nodeTreeToString(right)}")
              replaced ++= thedef.nonNull
              vr
            case _ =>
              node
          }
        case _ =>
          //println(s"ss no match ${nodeClassName(node)}")
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

  def nodeContainsRef(init: AST_Node, sym: SymbolDef) = {
    var found = false
    init.walk {
      case AST_SymbolRefDef(`sym`) =>
        found = true
        found
      case _ =>
        found
    }
    found
  }

  object ExtractVariables {
    def unapply(n: AST_Node): Option[Seq[(SymbolDef, AST_Node)]] = {
      val b = mutable.ArrayBuilder.make[(SymbolDef, AST_Node)]
      var assignsOnly = true
      n.walk {
        case AST_Assign(AST_SymbolRefDef(sym), "=", init) if !nodeContainsRef(init, sym) =>
          b += sym -> init
          true
        case _ =>
          assignsOnly = false
          true
      }
      val r = b.result()
      if (assignsOnly && r.nonEmpty) Some(r) else None
    }
  }
  /**
    * when possible, introduce a var into the for loop
    * i.e. transform for (i = 0; ..) {} into for (var i = 0; ..)
    */
  def detectForVars(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case f: AST_For =>
          val forOK: Seq[(SymbolDef, AST_Node, AST_Scope)] = f.init.nonNull.toSeq.flatMap {
            case _: AST_Definitions => // if init already is a definition, no need to process anything
              Seq()
            case ExtractVariables(vars) =>
              // we expect a sequence of variable initializations

              //println(s"Detect for with ${vars.map(_._1.name).mkString(",")}")
              // for each variable we need to verify the first use after the for loop is assignment
              // (or the variable is not used after the loop at all)
              // note: the assignment will often be in the init of another for loop
              val vScopes = for {
                (v, init) <- vars
                AST_Symbol(_, Defined(scope), _ ) <- v.orig.headOption
              } yield {
                (v, init, scope)
              }

              val forScopesOK = vScopes.forall { case (v, _, scope) =>
                // walk the scope, ignore references before the for, check first after the for
                var seenFor = false
                var seenAfterFor = false
                var seenAfterForInAssignment = false
                scope.walk {
                  case `f` =>
                    seenFor = true
                    true // no need to dive into the for
                  case AST_Assign(AST_SymbolRefDef(`v`), "=", init) if seenFor && !seenAfterFor && !nodeContainsRef(init, v) =>
                    //println(s"Seen ${v.name} after the for - in assignment")
                    seenAfterForInAssignment = true
                    seenAfterFor = true
                    true
                  case AST_SymbolRefDef(`v`) if seenFor =>
                    //println(s"Seen ${v.name} after the for - in use")
                    seenAfterFor = true
                    true
                  case _ =>
                    seenAfterFor

                }
                seenAfterForInAssignment || !seenAfterFor
              }

              if (forScopesOK) vScopes else Seq.empty
            case _ =>
              // something else than assignments into variables - leave it
              Seq()
          }
          if (forOK.nonEmpty) {
            //println("Transform for")

            val vars = forOK.map { case (v, initV, _) =>
                new AST_VarDef {
                  fillTokens(this, initV)
                  name = new AST_SymbolVar {
                    fillTokens(this, initV)
                    name = v.name
                    thedef = v
                    scope = v.scope // should be the for body instead, but it will be overwritten anyway
                  }
                  value = initV
                }
            }

            f.init = new AST_Let {
              /*_*/
              fillTokens(this, f)
              /*_*/
              definitions = vars.toJSArray
            }
            f
          } else {
            f
          }
        case _ =>
          node
      }
    }
    n

  }

  def instanceofImpliedCast(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case AST_If(AST_Binary(AST_SymbolRefDef(symDef), "instanceof", AST_SymbolRefName(cls)), _ , _) =>
          println(s"Implied cast $node")
          node
        case _ =>
          //println(s"No match $node")
          node

      }
    }
  }

}
