package com.github.opengrabeso

import Uglify._
import JsUtils._
/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  // low-level operations, used to implement the rest

  def add(n: AST_Node) = ???
  def remove(n: AST_Node) = ???

  // individual sensible transformations
  def detectVals(n: AST_Node): AST_Node = {
    val ret = n.clone()
    // walk the tree, check for possible val replacements and perform them
    val walker = new TreeWalker({ node =>
      node match {
        case v: AST_VarDef =>
          assert(v.name.thedef.map(_.name).get == v.name.name)
          if (v.name.init.nonNull.isEmpty) {
            println(s"Variable definition ${v.name.name} at ${v.start.map(_.line)},${v.start.map(_.col)}")
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
