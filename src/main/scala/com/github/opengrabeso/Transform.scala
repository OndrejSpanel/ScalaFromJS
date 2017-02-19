package com.github.opengrabeso

import Uglify._
import UglifyExt._
import JsUtils._
/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  // low-level operations, used to implement the rest

  def add(n: AST_Node) = ???
  def remove(n: AST_Node) = ???

  def checkAssignToReference(s: AST_SimpleStatement, df: SymbolDef) = {
    require(!s.body.isInstanceOf[AST_Statement])
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
  def detectVals(n: AST_Node): AST_Node = {
    val ret = n.clone()
    // walk the tree, check for possible val replacements and perform them
    val walker = new TreeWalker({ node =>
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
                ref.scope.exists { scope =>
                  scope.body.exists {
                    case ss: AST_SimpleStatement =>
                      checkAssignToReference(ss, df)
                    case bs: AST_BlockStatement => // block statement is another scope, should be already handled
                      false
                    case s =>
                      false
                  }
                }
              }
              println(s"${df.name} is ${df._isVal}")
            }
          }
        case _ =>
      }
      false
    })
    ret.walk(walker)
    ret
  }

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Node): AST_Node = n

  def apply(n: AST_Toplevel): AST_Toplevel = {
    val init = varInitialization(n).asInstanceOf[AST_Toplevel]
    val vals = detectVals(init).asInstanceOf[AST_Toplevel]
    vals
  }
}
