package com.github.opengrabeso

import net.gamatron.esprima._
import esprima._

import Symbols._

object Casting {
  class InstanceOf(op: String) {
    def unapply(arg: Node.BinaryExpression) = arg match {
      // a && (a op b)
      case Node.BinaryExpression(Node.SymbolRefDef(sym), "&&",Node.BinaryExpression(Node.SymbolRefDef(symDef), `op`, cs: Node.SymbolRef)) if sym == symDef =>
        Some(symDef, cs)
      // a op b
      case Node.BinaryExpression(Node.SymbolRefDef(symDef), `op`, cs: Node.SymbolRef) =>
        Some(symDef, cs)
      case _ =>
        None
    }
  }

  class InstanceOfCondition(op: String) {
    self => // recursive extractor cannot use this

    object InstanceOf extends InstanceOf(op)

    def unapply(arg: Node.BinaryExpression): Option[(SymbolDef, Seq[Node.SymbolRef])] = arg match {
      case Node.BinaryExpression(InstanceOf(symDef, cs), "||", self(symDef2, cond)) if symDef == symDef2 =>
        //println(s"$op Node.BinaryExpression")
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