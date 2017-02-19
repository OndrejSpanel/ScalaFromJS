package com.github.opengrabeso

import Uglify._

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  // low-level operations, used to implement the rest

  def add(n: AST_Node) = ???
  def remove(n: AST_Node) = ???

  // individual sensible transformations
  def detectVals(n: AST_Node): AST_Node = ???

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Node): AST_Node = ???

  def apply(n: AST_Toplevel): AST_Toplevel = {
    val init = varInitialization(n).asInstanceOf[AST_Toplevel]
    val vals = detectVals(init).asInstanceOf[AST_Toplevel]
    vals
  }
}
