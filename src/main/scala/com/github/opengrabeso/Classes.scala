package com.github.opengrabeso

import Transform._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._

import scala.scalajs.js.RegExp
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

  def findThisClassInWalker(walker: TreeWalker): Option[AST_DefClass] = {
    //println(walker.stack.map(nodeClassName).mkString(":"))
    walker.stack.reverse.collectFirst {
      case c: AST_DefClass =>
        //println(s"Found ${c.name.map(_.name)}")
        c
    }
  }

    // ignore function scopes, find a class one
  def findThisClass(scope: Option[AST_Scope]): Option[AST_DefClass] = {
    //println(s"  ${scope.map(nodeClassName)} ${scope.map(_.nesting)}")
    scope match {
      case Some(s: AST_DefClass) => Some(s)
      case Some(x) =>
        val s = x.parent_scope.nonNull
        findThisClass(s)
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
    val thisScope = findThisClass(scope)
    thisScope.flatMap(superClass)
  }

  def getParent(clazz: AST_DefClass): Option[AST_DefClass] = {
    clazz.`extends`.nonNull.collect {
      case c: AST_DefClass => c
    }
  }


  def includeParents(clazz: AST_DefClass, ret: Seq[AST_DefClass])(ctx: ExpressionTypeContext): Seq[AST_DefClass] = {
    clazz.`extends`.nonNull match {
      case Some(cls: AST_SymbolRef) =>
        val c = ctx.classes.get(cls.name)
        c.fold(ret)(parent => includeParents(parent, parent +: ret)(ctx))
      case _ => ret
    }
  }

  def getParents(tpe: String)(ctx: ExpressionTypeContext): Seq[String] = {
    ctx.classInfo.listParents(tpe)
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
      val newInlineBody = newMethod(inlineBodyName, Seq(), Seq(), cls)
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

  def replaceProperty(c: AST_DefClass, oldP: AST_ObjectProperty, newP: AST_ObjectProperty): AST_DefClass = {
    c.properties = c.properties.map(p => if (p == oldP) newP else p)
    c
  }

  def deleteVarMember(c: AST_DefClass, member: RegExp) = {
    val inlineBody = Classes.findInlineBody(c)
    inlineBody.fold(c) { ib =>
      // filter member variables as well
      val retIB = ib.clone()
      retIB.value.body = retIB.value.body.filterNot {
        case AST_Definitions(AST_VarDef(AST_SymbolName(v), _)) if member.test(v) =>
          true
        case _ =>
          false
      }
      Classes.replaceProperty(c, ib, retIB)
    }
  }


  def transformClassParameters(c: AST_DefClass, init: AST_Node): AST_Node = {
    val transformed = for (cons <- findConstructor(c)) yield {
      init.transformAfter { (node, transformer) =>
        node match {
          case sym@AST_SymbolRefName(name) =>
            val pn = cons.value.argnames.find(_.name == name)
            pn.fold(sym) { p =>
              val c = sym.clone()
              c.name = c.name + SymbolTypes.parSuffix
              c
            }
          case _ =>
            node
        }
      }
    }
    transformed.getOrElse(init)
  }


}
