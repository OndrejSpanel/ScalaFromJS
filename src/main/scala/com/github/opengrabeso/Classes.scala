package com.github.opengrabeso

import Transform._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._
import scalajs.js

object Classes {

  def findDefScope(scope: Option[AST_Scope]): Option[AST_Scope] = {
    //println(s"  ${scope.map(nodeClassName)} ${scope.map(_.nesting)}")
    scope match {
      case Some(s: AST_DefClass) => Some(s)
      case Some(f: AST_Lambda) => Some(f)
      case Some(x) =>
        val s = x.parent_scope.nonNull
        findDefScope(s)
      case _ => None
    }
  }

  // TODO: rename to findClassScope
  def findThisScope(scope: Option[AST_Scope]): Option[AST_DefClass] = {
    findDefScope(scope).collect {
      case c: AST_DefClass => c
    }
  }

  def findThisFunction(scope: Option[AST_Scope]): Option[AST_Lambda] = {
    findDefScope(scope).collect {
      case c: AST_Lambda => c
    }
  }

  // ignore function scopes, find a class one
  def findThisClass(scope: Option[AST_Scope]): Option[AST_DefClass] = {
    scope match {
      case Some(s: AST_DefClass) => Some(s)
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

  val isConstructorProperty: PartialFunction[AST_ObjectProperty, AST_ConciseMethod] = {
    case m: AST_ConciseMethod if m.key.name == "constructor" =>
      m
  }

  def findConstructor(c: AST_DefClass): Option[AST_ConciseMethod] = {
    c.properties.collectFirst(isConstructorProperty)
  }

  val inlineBodyName = "inline_^"

  def findInlineBody(c: AST_DefClass):  Option[AST_ConciseMethod] = {
    findMethod(c, inlineBodyName)

  }

  def classInlineBody(cls: AST_DefClass): AST_Accessor = {
    val present = findInlineBody(cls)
    val method = present.getOrElse {
      val newInlineBody = new AST_ConciseMethod {
        fillTokens(this, cls)

        key = new AST_SymbolRef {
          fillTokens(this, cls)
          name = inlineBodyName
        }
        value = new AST_Accessor {
          fillTokens(this, cls)
          argnames = js.Array()
          this.body = js.Array()
        }
      }
      cls.properties = cls.properties :+ newInlineBody
      newInlineBody
    }
    method.value
  }



  def findMethod(c: AST_DefClass, name: String): Option[AST_ConciseMethod] = {
    c.properties.collectFirst {
      case m: AST_ConciseMethod if m.key.name == name => m
    }
  }

  def findProperty(c: AST_DefClass, name: String): Option[AST_ObjectKeyVal] = {
    c.properties.collectFirst {
      case m: AST_ObjectKeyVal if m.key == name => m
    }
  }



}
