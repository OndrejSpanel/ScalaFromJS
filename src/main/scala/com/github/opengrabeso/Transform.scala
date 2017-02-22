package com.github.opengrabeso

import com.github.opengrabeso.JsUtils._
import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt._
import com.github.opengrabeso.UglifyExt.Import._

import scala.scalajs.js

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  // individual sensible transformations

  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Node): AST_Node = {
    val ret = n.clone()
    // walk the tree, check for possible val replacements and perform them
    ret.walk {
      case AST_VarDef(name, value) if value.nonNull.nonEmpty => // var with init - search for a modification
         for (df <- name.thedef) {
          assert(df.name == name.name)
          // TODO: infer type
          // check if any reference is assignment target
          val assignedInto = df.references.exists { ref =>
            //println(s"Reference to ${df.name} in scope ${ref.scope.get.nesting}")
            assert(ref.thedef.exists(_ == df))
            ref.scope.exists { s =>
              var detect = false
              object UnaryModification {
                def unapply(arg: String): Boolean = arg == "++" || arg == "--"
              }
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
          df._isVal = !assignedInto
        }
        false
      case _ =>
        false
    }
    ret
  }

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Node): AST_Node = {

    // walk the tree, check for first reference of each var
    var pairs = Map.empty[SymbolDef, AST_SymbolRef]
    n.walk { node =>
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
    val changeAssignToVar = n.transformAfter {(node, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      //println(s"node ${nodeClassName(node)} ${ScalaOut.outputNode(node, "")}")
      // we need to descend into assignment definitions, as they may contain other assignments
      //val nodeAdj = node.clone()
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
              node //.clone()
          }
        case c =>
          node //.clone()
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
          val vv = v.clone().asInstanceOf[AST_Var]
          vv.definitions = af
          vv
        case c =>
          c
      }
    }


    ret
  }

  def handleIncrement(n: AST_Node): AST_Node = {

    object UnaryModification {
      def unapply(arg: String): Boolean = arg == "++" || arg == "--"
    }

    // walk the tree, check for increment / decrement
    val t = n.transformAfter { (node, transformer) =>
      node match {
        case AST_Unary(op@UnaryModification(), expr@AST_SymbolRef(name, scope, thedef)) =>
          println("Match unary")
          transformer.parent() match {
            case ss: AST_SimpleStatement =>
              println("Replaced")
              val c = ss.clone().asInstanceOf[AST_SimpleStatement]
              c.body = new AST_Assign {
                left = expr
                operator = "+="
                right = new AST_Number {
                  value = 1
                }
              }
              c
            case _ =>
              node

          }
        case _ =>
          node
      }
    }


    t
  }

  def apply(n: AST_Toplevel): AST_Toplevel = {

    val transforms: Seq[(AST_Node) => AST_Node] = Seq(
      handleIncrement,
      varInitialization,
      detectVals
    )

    // beware: we must not call figure_out_scope after detecting vals, it destroys the val information
    transforms.foldLeft(n) { (t,op) =>
      t.figure_out_scope()
      val r = op(t).asInstanceOf[AST_Toplevel]
      r
    }
  }
}
