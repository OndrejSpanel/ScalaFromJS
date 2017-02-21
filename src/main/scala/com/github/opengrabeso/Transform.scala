package com.github.opengrabeso

import com.github.opengrabeso.JsUtils._
import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt._

import scala.scalajs.js

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  // low-level operations, used to implement the rest

  def add(n: AST_Node) = ???
  def remove(n: AST_Node) = ???

  def checkAssignToReference(s: AST_SimpleStatement, df: SymbolDef) = {
    //assert(!s.body.isInstanceOf[AST_Statement]) // as per documentation of AST_SimpleStatement
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

  def checkUnaryToReference(s: AST_Unary, df: SymbolDef) = {
    s.expression match {
      case sym: AST_SymbolRef =>
        (s.operator == "++" || s.operator == "--") && sym.thedef.exists(_ == df)
      case _ =>
        false
    }
  }

  // individual sensible transformations

  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Node): AST_Node = {
    val ret = n.clone()
    // walk the tree, check for possible val replacements and perform them
    ret.walk{ node: AST_Node =>
      node match {
        case v: AST_VarDef =>
          for (df <- v.name.thedef) {
            assert(df.name == v.name.name)
            if (v.name.init.nonNull.isEmpty) {
              // without init it must be var -
              // TODO: infer type

              // check if any reference is assignment target
              df._isVal = !df.references.exists { ref =>
                assert(ref.thedef.exists(_ == df))
                ref.scope.exists{ s =>
                  var detect = false
                  //noinspection MatchToPartialFunction
                  s.walk { node: AST_Node =>
                    node match {
                      case s: AST_Scope => s != ref.scope // do not descend into any other scopes, they are listed in references if needed
                      case ss: AST_SimpleStatement =>
                        if (checkAssignToReference(ss, df)) detect = true
                        detect
                      case u: AST_Unary =>
                        if (checkUnaryToReference(u, df)) detect = true
                        detect
                      case _ =>
                        detect
                    }
                  }
                  detect
                }
              }
            }
          }
        case _ =>
      }
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
                if (firstRef.scope == df.scope) {
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

    // walk the tree, check for possible val replacements and perform them
    val changeAssignToVar = n.transform { (node, descend, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      node match {
        case vd: AST_Assign if vd.operator == "=" =>
          vd.left match {
            case sr: AST_SymbolRef if refs contains sr =>
              val stackTail = transformer.stack.takeRight(3).toSeq
              stackTail match {
                case Seq(_: AST_Block, _: AST_SimpleStatement, _: AST_Assign) =>
                  val parent = transformer.parent()
                  val vr = new AST_Var
                  val vv = new AST_VarDef
                  vr.definitions = js.Array(vv)
                  vv.name = new AST_SymbolVar
                  vv.name.thedef = sr.thedef
                  vv.name.name = sr.name
                  vv.value = vd.right
                  vv.name.scope = sr.scope
                  // TODO: we should replace AST_SimpleStatement with AST_Definitions
                  println(s"Replaced ${vv.name.name} AST_SymbolRef with AST_VarDef")
                  vr
                case _ =>
                  pairs -= sr.thedef.get
                  sr //.clone()
              }
            case _ =>
              vd //.clone()
          }
        case c =>
          val cc = c //.clone()
          descend(cc, transformer)
          cc
      }
    }

    // walk the tree, check for possible val replacements and perform them
    val ret = changeAssignToVar.transform { (node, descend, transformer) =>
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
          val cc = node.clone()
          descend(cc, transformer)
          cc
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
