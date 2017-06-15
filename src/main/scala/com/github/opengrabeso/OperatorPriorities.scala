package com.github.opengrabeso

object OperatorPriorities {
  def useParens(op: String, outer: String, right: Boolean) = {
    if (op != outer) needsParens(outer, op, right)
    else right && nonAssociative.contains(op)
  }

  val priorityOrderings = Seq(
    Seq(Set("+", "-"),Set("*", "/", "%")),
    Seq(Set("|","^"), Set("&"))
  )

  val nonAssociative = Set("/", "-")

  def needsParens(outer: String, inner: String, right: Boolean): Boolean = {
    //println(s"outer $outer inner $inner")
    val innerIsLowerPrio = priorityOrderings.exists { ordering =>
      val lower = ordering.dropWhile(!_.contains(inner)).drop(1)
      lower.exists(_.contains(outer))
    }
    def innerIsSamePrio = priorityOrderings.exists { ordering =>
      ordering.exists(o => o.contains(inner) && o.contains(outer))
    }
    //println(s"outer $outer inner $inner => $innerIsLowerPrio")
    innerIsLowerPrio || right && (nonAssociative contains outer) && innerIsSamePrio
  }

}
