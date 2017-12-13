package com.github.opengrabeso

import com.github.opengrabeso.esprima._
import _root_.esprima._
import Symbols._
import com.github.opengrabeso.esprima.symbols._

object Casting {
  class InstanceOf(op: String) {
    def unapply(arg: Node.BinaryExpression)(implicit context: ScopeContext): Option[(SymId, Node.Identifier)] = arg match {
      // a && (a op b)
      case Node.BinaryExpression("&&", Node.Identifier(Id(sym)), Node.BinaryExpression(`op`, Node.Identifier(Id(symDef)), cs: Node.Identifier)) if sym == symDef =>
        Some(symDef, cs)
      // a op b
      case Node.BinaryExpression(`op`, Node.Identifier(Id(symDef)), cs: Node.Identifier) =>
        Some(symDef, cs)
      case _ =>
        None
    }
  }

  class InstanceOfCondition(op: String) {
    self => // recursive extractor cannot use this

    object InstanceOf extends InstanceOf(op)

    def unapply(arg: Node.BinaryExpression)(implicit context: ScopeContext): Option[(SymId, Seq[Node.Identifier])] = arg match {
      case Node.BinaryExpression("||", InstanceOf(symDef, cs), self(symDef2, cond)) if symDef == symDef2 =>
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