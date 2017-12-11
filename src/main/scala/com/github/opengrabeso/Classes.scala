package com.github.opengrabeso

import Transform._
import net.gamatron.esprima._


import JsUtils._
import SymbolTypes._

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

  def superClassSymbolDef(cls: AST_DefClass): Option[SymbolDef] = {
    cls.`extends`.nonNull.collect {
      case AST_SymbolRefDef(c) =>
        // symbol defined, use it directly
        //println(s"  AST_SymbolRefDef ${c.name}")
        Some(c)
      case AST_SymbolRef(name, _, _) =>
        // symbol not defined, search for it
        //println(s"  AST_SymbolRef $name $scope $thedef")

        val thisCls = cls
        val superSym = for {
          thisClsSymbol <- thisCls.name.nonNull
          scope <- thisClsSymbol.scope.nonNull
          scopeSymbols <- scope.enclosed.nonNull
          baseSym <- scopeSymbols.find(_.name == name)
        } yield {
          baseSym
        }
        superSym
    }.flatten
  }

  def superClass(cls: AST_DefClass): Option[SymbolMapId] = {
    //println(s"superClass ${cls.name.get.name}")

    val baseSym = superClassSymbolDef(cls)

    val baseId = baseSym.flatMap(SymbolTypes.id)

    //println(s"  baseSym ${baseSym.map(_.name)} baseId $baseId")

    baseId
  }

  def findSuperClass(scope: Option[AST_Scope]): Option[SymbolMapId] = {
    val thisScope = findThisClass(scope)
    thisScope.flatMap(superClass)
  }

  def getClassId(cls: AST_DefClass): Option[Int] = {
    val sid = cls.name.nonNull.flatMap(_.thedef.nonNull.flatMap(id))
    sid.map(_.sourcePos)
  }


  def includeParents(clazz: AST_DefClass, ret: Seq[AST_DefClass])(ctx: ExpressionTypeContext): Seq[AST_DefClass] = {
    clazz.`extends`.nonNull match {
      case Some(cls: AST_SymbolRef) =>
        val c = cls.thedef.nonNull.flatMap(id).flatMap(ctx.classes.get)
        c.fold(ret) { parent =>
          if (ret contains parent) {
            //println(s"includeParents: Prevented recursion for $parent in $ret")
            ret
          }
          else includeParents(parent, parent +: ret)(ctx)
        }
      case _ => ret
    }
  }

  def getParents(tpe: SymbolMapId)(ctx: ExpressionTypeContext): Seq[SymbolMapId] = {
    ctx.classInfo.listParents(tpe)
  }


  def findInParents(tpe: SymbolMapId, member: String)(ctx: ExpressionTypeContext): Option[SymbolMapId] = {
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

  def classInlineBody(cls: AST_DefClass, tokensFrom: AST_Node): AST_Accessor = {
    //println(s"Class inline body $cls")
    val present = findInlineBody(cls)
    val method = present.getOrElse {
      val newInlineBody = newMethod(inlineBodyName, Seq(), Seq(), tokensFrom)
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

  def propertyIsStatic(prop: AST_ObjectProperty): Boolean = prop match {
    case m: AST_ConciseMethod => m.`static`
    case m: AST_ObjectSetter => m.`static`
    case m: AST_ObjectGetter => m.`static`
    case m: AST_ObjectKeyVal => m.quote == "'"
    case _ => false
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
              c.name = c.name + Symbols.parSuffix
              //println(s"transformClassParameters ${c.name}")
              c
            }
          case _ =>
            node
        }
      }
    }
    transformed.getOrElse(init)
  }


  def classListHarmony(n: AST_Extended) = {
    var classes = Map.empty[SymbolMapId, AST_DefClass]
    n.top.walk {
      case d: AST_DefClass =>
        for {
          name <- d.name
          id <- name.thedef.nonNull.flatMap(id)
        } {
          classes += id -> d
        }
        true
      case _ : AST_Toplevel =>
        false
      case _ =>
        false
    }
    classes
  }

  case class ClassListHarmony(classes: Map[SymbolMapId, AST_DefClass]) {

    def this(n: AST_Extended) = this(classListHarmony(n))

    def get(name: SymbolMapId): Option[AST_DefClass] = classes.get(name)

    def classPos(name: SymbolMapId): Int = name.sourcePos

  }

}
