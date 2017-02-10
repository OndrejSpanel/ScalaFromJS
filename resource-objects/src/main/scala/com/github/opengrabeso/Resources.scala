package com.github.opengrabeso

import language.experimental.macros

import reflect.macros.blackbox

object Resources {

  def getResource(name: String): String = macro getResource_impl

  def getResource_impl(c: blackbox.Context)(name: c.Expr[String]) = {
    import c.universe._
    val nameStr = name match {
      case Expr(Literal(Constant(nValue: String))) =>
        nValue
      case _ =>
        c.abort(c.enclosingPosition, "String literal expected")
    }

    c.parse("resources.`" + nameStr + "`.str")
  }
}
