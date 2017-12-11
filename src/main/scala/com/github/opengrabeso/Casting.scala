package com.github.opengrabeso

import net.gamatron.esprima._
import esprima._

import Symbols._

object Casting {
  class InstanceOf(op: String) {
    def unapply(arg: Node.Binary) = arg match {
      // a && (a op b)
      case Node.Binary(Node.SymbolRefDef(sym), "&&",Node.Binary(Node.SymbolRefDef(symDef), `op`, cs: Node.SymbolRef)) if sym == symDef =>
        Some(symDef, cs)
      // a op b
      case Node.Binary(Node.SymbolRefDef(symDef), `op`, cs: Node.SymbolRef) =>
        Some(symDef, cs)
      case _ =>
        None
    }
  }

  class InstanceOfCondition(op: String) {
    self => // recursive extractor cannot use this

    object InstanceOf extends InstanceOf(op)

    def unapply(arg: Node.Binary): Option[(SymbolDef, Seq[Node.SymbolRef])] = arg match {
      case Node.Binary(InstanceOf(symDef, cs), "||", self(symDef2, cond)) if symDef == symDef2 =>
        //println(s"$op Node.Binary")
        Some(symDef, cs +: cond)
      case InstanceOf(symDef, cs) =>
        //println(s"$op InstanceOf")
        Some(symDef, Seq(cs))
      case _ =>
        //println(s"$op None $arg")
        None
    }
  }

  object InstanceOfCondition extends InstanceOfCondition(instanceof)

}