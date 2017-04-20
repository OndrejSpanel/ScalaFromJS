package com.github.opengrabeso

import Uglify._
import UglifyExt.Import._

import Symbols._

object Casting {
  class InstanceOf(op: String) {
    def unapply(arg: AST_Binary) = arg match {
      // a && (a op b)
      case AST_Binary(AST_SymbolRefDef(sym), "&&",AST_Binary(AST_SymbolRefDef(symDef), `op`, cs: AST_SymbolRef)) if sym == symDef =>
        Some(symDef, cs)
      // a op b
      case AST_Binary(AST_SymbolRefDef(symDef), `op`, cs: AST_SymbolRef) =>
        Some(symDef, cs)
      case _ =>
        None
    }
  }

  class InstanceOfCondition(op: String) {
    self => // recursive extractor cannot use this

    object InstanceOf extends InstanceOf(op)

    def unapply(arg: AST_Binary): Option[(SymbolDef, Seq[AST_SymbolRef])] = arg match {
      case AST_Binary(InstanceOf(symDef, cs), "||", self(symDef2, cond)) if symDef == symDef2 =>
        //println(s"$op AST_Binary")
        Some(symDef, cs +: cond)
      case InstanceOf(symDef, cs) =>
        //println(s"$op InstanceOf")
        Some(symDef, Seq(cs))
      case _ =>
        //println(s"$op None")
        None
    }
  }

  object InstanceOfCondition extends InstanceOfCondition(`instanceof`)

}