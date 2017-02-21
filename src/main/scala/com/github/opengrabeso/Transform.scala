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

  // low-level operations, used to implement the rest

  def add(n: AST_Node) = ???
  def remove(n: AST_Node) = ???

  def checkAssignToReference(s: AST_SimpleStatement, df: SymbolDef) = {
    assert(!s.body.isInstanceOf[AST_Statement]) // as per documentation of AST_SimpleStatement
    s.body match {
      case a: AST_Assign =>
        a.left match {
          case sym: AST_SymbolRef =>
            sym.thedef.exists(_ == df)
          case _ =>
            false
        }
      case a =>
        false
    }
  }

  // individual sensible transformations

  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Node): AST_Node = {
    val ret = n.clone()
    // walk the tree, check for possible val replacements and perform them
    ret.walk {
      case AST_VarDef(name, value) if name.init.nonNull.isEmpty => // var with no init - search for the init
        for (df <- name.thedef) {
          assert(df.name == name.name)
          // TODO: infer type
          // check if any reference is assignment target
          df._isVal = !df.references.exists { ref =>
            assert(ref.thedef.exists(_ == df))
            ref.scope.exists { s =>
              var detect = false
              s.walk {
                case s: AST_Scope => s != ref.scope // do not descend into any other scopes, they are listed in references if needed
                case ss: AST_SimpleStatement =>
                  if (checkAssignToReference(ss, df)) detect = true
                  detect
                case AST_Unary(op, AST_SymbolRef(nameSym, scope, `df`)) if op == "--" || op == "++" =>
                  detect = true
                  detect
                case _ =>
                  detect
              }
              detect
            }
          }
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
        case v: AST_VarDef =>
          if (v.value.nonNull.isEmpty) {
            for (df <- v.name.thedef) {
              assert(df.name == v.name.name)
              if (df.references.nonEmpty) {
                // find the first reference
                val firstRef = df.references.minBy { ref =>
                  assert(ref.thedef.exists(_ == df))
                  ref.start.map(_.pos).getOrElse(Int.MaxValue)
                }
                // if the first ref is in the current scope, we might merge it with the declaration
                if (firstRef.scope == v.name.scope) {
                  pairs += df -> firstRef
                }

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

  def apply(n: AST_Toplevel): AST_Toplevel = {
    val init = varInitialization(n).asInstanceOf[AST_Toplevel]
    init.figure_out_scope()
    val vals = detectVals(init).asInstanceOf[AST_Toplevel]
    vals
  }
}
