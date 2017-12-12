package com.github.opengrabeso

import Transform._
import net.gamatron.esprima._
import esprima._

import JsUtils._
import SymbolTypes._

object Classes {


  def findDefScope(scope: Option[Node.Scope]): Option[Node.Scope] = {
    //println(s"  ${scope.map(nodeClassName)} ${scope.map(_.nesting)}")
    scope match {
      case Some(s: Node.DefClass) => Some(s)
      case Some(f: Node.Lambda) => Some(f)
      case Some(x) =>
        val s = x.parent_scope
        findDefScope(s)
      case _ => None
    }
  }

  // TODO: rename to findClassScope
  def findThisScope(scope: Option[Node.Scope]): Option[Node.DefClass] = {
    findDefScope(scope).collect {
      case c: Node.DefClass => c
    }
  }

  def findThisFunction(scope: Option[Node.Scope]): Option[Node.Lambda] = {
    findDefScope(scope).collect {
      case c: Node.Lambda => c
    }
  }

  def findThisClassInWalker(walker: TreeWalker): Option[Node.DefClass] = {
    //println(walker.stack.map(nodeClassName).mkString(":"))
    walker.stack.reverse.collectFirst {
      case c: Node.DefClass =>
        //println(s"Found ${c.name.map(_.name)}")
        c
    }
  }

    // ignore function scopes, find a class one
  def findThisClass(scope: Option[Node.Scope]): Option[Node.DefClass] = {
    //println(s"  ${scope.map(nodeClassName)} ${scope.map(_.nesting)}")
    scope match {
      case Some(s: Node.DefClass) => Some(s)
      case Some(x) =>
        val s = x.parent_scope
        findThisClass(s)
      case _ => None
    }
  }

  def superClassSymbolDef(cls: Node.DefClass): Option[SymbolDef] = {
    cls.`extends`.collect {
      case Node.Identifier(c) =>
        // symbol defined, use it directly
        //println(s"  Node.Identifier ${c.name}")
        Some(c)
      case Node.Identifier(name, _, _) =>
        // symbol not defined, search for it
        //println(s"  Node.Identifier $name $scope $thedef")

        val thisCls = cls
        val superSym = for {
          thisClsSymbol <- thisCls.name
          scope <- thisClsSymbol.scope
          scopeSymbols <- scope.enclosed
          baseSym <- scopeSymbols.find(_.name == name)
        } yield {
          baseSym
        }
        superSym
    }.flatten
  }

  def superClass(cls: Node.DefClass): Option[SymbolMapId] = {
    //println(s"superClass ${cls.name.get.name}")

    val baseSym = superClassSymbolDef(cls)

    val baseId = baseSym.flatMap(SymbolTypes.id)

    //println(s"  baseSym ${baseSym.map(_.name)} baseId $baseId")

    baseId
  }

  def findSuperClass(scope: Option[Node.Scope]): Option[SymbolMapId] = {
    val thisScope = findThisClass(scope)
    thisScope.flatMap(superClass)
  }

  def getClassId(cls: Node.DefClass): Option[Int] = {
    val sid = cls.name.flatMap(_.thedef.flatMap(id))
    sid.map(_.sourcePos)
  }


  def includeParents(clazz: Node.DefClass, ret: Seq[Node.DefClass])(ctx: ExpressionTypeContext): Seq[Node.DefClass] = {
    clazz.`extends` match {
      case Some(cls: Node.Identifier) =>
        val c = cls.thedef.flatMap(id).flatMap(ctx.classes.get)
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
      parent@Node.DefClass(Defined(Node.SymbolName(c)), _, _) <- includeParents(clazz, Seq(clazz))(ctx)
      ... search parent
    } {
      return Some(c)
    }
    None
    */
  }

  val isConstructorProperty: PartialFunction[Node.ObjectProperty, Node.ConciseMethod] = {
    case m: Node.ConciseMethod if m.key.name == "constructor" =>
      m
  }

  def findConstructor(c: Node.DefClass): Option[Node.ConciseMethod] = {
    c.properties.collectFirst(isConstructorProperty)
  }

  val inlineBodyName = "inline_^"

  def findInlineBody(c: Node.DefClass):  Option[Node.ConciseMethod] = {
    findMethod(c, inlineBodyName)

  }

  def classInlineBody(cls: Node.DefClass, tokensFrom: Node.Node): Node.Accessor = {
    //println(s"Class inline body $cls")
    val present = findInlineBody(cls)
    val method = present.getOrElse {
      val newInlineBody = newMethod(inlineBodyName, Seq(), Seq(), tokensFrom)
      cls.properties = cls.properties :+ newInlineBody
      newInlineBody
    }
    method.value
  }



  def findMethod(c: Node.DefClass, name: String): Option[Node.ConciseMethod] = {
    c.properties.collectFirst {
      case m: Node.ConciseMethod if m.key.name == name => m
    }
  }

  def findProperty(c: Node.DefClass, name: String): Option[Node.ObjectKeyVal] = {
    c.properties.collectFirst {
      case m: Node.ObjectKeyVal if m.key == name => m
    }
  }

  def propertyIsStatic(prop: Node.ObjectProperty): Boolean = prop match {
    case m: Node.ConciseMethod => m.`static`
    case m: Node.ObjectSetter => m.`static`
    case m: Node.ObjectGetter => m.`static`
    case m: Node.ObjectKeyVal => m.quote == "'"
    case _ => false
  }

  def replaceProperty(c: Node.DefClass, oldP: Node.ObjectProperty, newP: Node.ObjectProperty): Node.DefClass = {
    c.properties = c.properties.map(p => if (p == oldP) newP else p)
    c
  }

  def deleteVarMember(c: Node.DefClass, member: RegExp) = {
    val inlineBody = Classes.findInlineBody(c)
    inlineBody.fold(c) { ib =>
      // filter member variables as well
      val retIB = ib.clone()
      retIB.value.body = retIB.value.body.filterNot {
        case Node.Definitions(Node.VarDef(Node.SymbolName(v), _)) if member.test(v) =>
          true
        case _ =>
          false
      }
      Classes.replaceProperty(c, ib, retIB)
    }
  }


  def transformClassParameters(c: Node.DefClass, init: Node.Node): Node.Node = {
    val transformed = for (cons <- findConstructor(c)) yield {
      init.transformAfter { (node, transformer) =>
        node match {
          case sym@Node.Identifier(name) =>
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


  def classListHarmony(n: NodeExtended) = {
    var classes = Map.empty[SymbolMapId, Node.DefClass]
    n.top.walk {
      case d: Node.DefClass =>
        for {
          name <- d.name
          id <- name.thedef.flatMap(id)
        } {
          classes += id -> d
        }
        true
      case _ : Node.Program =>
        false
      case _ =>
        false
    }
    classes
  }

  case class ClassListHarmony(classes: Map[SymbolMapId, Node.DefClass]) {

    def this(n: NodeExtended) = this(classListHarmony(n))

    def get(name: SymbolMapId): Option[Node.DefClass] = classes.get(name)

    def classPos(name: SymbolMapId): Int = name.sourcePos

  }

}
