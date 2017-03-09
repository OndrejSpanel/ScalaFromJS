package com.github.opengrabeso

import Transform._
import Uglify._
import UglifyExt.Import._
import JsUtils._

object Classes {

  def findThisScope(scope: Option[AST_Scope]): Option[AST_DefClass] = {
    scope match {
      case Some(s: AST_DefClass) => Some(s)
      case Some(_: AST_Function) => None // do functions define a different this for functions?
      case Some(x) =>
        val s = x.parent_scope.nonNull
        findThisScope(s)
      case _ => None
    }
  }

  def superClass(cls: AST_DefClass): Option[String] = {
    val sup = cls.`extends`.nonNull.collect {
      case AST_SymbolRefName(c) =>
        c
    }
    sup
  }

  def findSuperClass(scope: Option[AST_Scope])(ctx: ExpressionTypeContext): Option[String] = {
    val thisScope = findThisScope(scope)
    thisScope.flatMap(superClass)
  }

  def includeParents(clazz: AST_DefClass, ret: Seq[AST_DefClass])(ctx: ExpressionTypeContext): Seq[AST_DefClass] = {
    clazz.`extends`.nonNull match {
      case Some(cls: AST_SymbolRef) =>
        val c = ctx.classes.get(cls.name)
        c.fold(ret)(parent => includeParents(parent, parent +: ret)(ctx))
      case _ => ret
    }
  }

  def findInParents(tpe: String, member: String)(ctx: ExpressionTypeContext): Option[String] = {
    ctx.classInfo.classContains(tpe, member)
    /*
    for {
      clazz <- ctx.classes.get(tpe)
      parent@AST_DefClass(Defined(AST_SymbolName(c)), _, _) <- includeParents(clazz, Seq(clazz))(ctx)
      ... search parent
    } {
      return Some(c)
    }
    None
    */
  }



}
