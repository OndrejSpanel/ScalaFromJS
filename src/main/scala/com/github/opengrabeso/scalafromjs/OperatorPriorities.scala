package com.github.opengrabeso.scalafromjs

object OperatorPriorities {
  def useParens(op: String, outer: String, right: Boolean) = {
    if (op != outer) needsParens(outer, op, right)
    else right && nonAssociative.contains(op)
  }

  val assignments = Set("=", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%=")
  val dot = Set(Symbols.instanceof, Symbols.asinstanceof)
  val comparisons = Seq(Set("||"), Set("&&"), Set("==", "!=", "<=", ">=", "<", ">"))

  val priorityOrderings = Seq(
    Seq(assignments) ++ comparisons ++ Seq(Set("+", "-"),Set("*", "/", "%"), dot),
    Seq(assignments, Set("|"), Set("^"), Set("&"), dot),
    Seq(assignments, Set("<<", ">>"), dot)
  )

  val nonAssociative = Set("/", "-", "%")

  def findOrdering(op: String): Option[Seq[Set[String]]] = {
    priorityOrderings.find(_.exists(_.contains(op)))
  }

  def needsParens(outer: String, inner: String, right: Boolean): Boolean = {
    //println(s"outer $outer inner $inner")
    val innerIsLowerPrio = priorityOrderings.exists { ordering =>
      val lower = ordering.dropWhile(!_.contains(inner)).drop(1)
      lower.exists(_.contains(outer))
    }
    def innerIsSamePrio = priorityOrderings.exists { ordering =>
      ordering.exists(o => o.contains(inner) && o.contains(outer))
    }
    def sameOrdering(op1: String, op2: String) = {
      priorityOrderings.exists(o => o.exists(_ contains op1) && o.exists(_ contains op2))
    }
    //println(s"outer $outer inner $inner => $innerIsLowerPrio")
    innerIsLowerPrio || right && (nonAssociative contains outer) && innerIsSamePrio || !sameOrdering(outer, inner)
  }

}
