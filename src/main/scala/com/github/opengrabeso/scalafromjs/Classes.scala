package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import JsUtils._
import SymbolTypes._
import com.github.opengrabeso.scalafromjs.Transform.ExpressionTypeContext
import com.github.opengrabeso.scalafromjs.esprima.symbols._

import scala.util.matching.Regex

object Classes {


  def findDefScope(scope: Option[Node.Node])(implicit context: ScopeContext): Option[Node.Node] = {
    //println(s"  ${scope.map(nodeClassName)} ${scope.map(_.nesting)}")
    scope match {
      case Some(s@IsDeclScope()) => Some(s)
      case Some(x) =>
        val s = ???
        findDefScope(s)
      case _ => None
    }
  }

  // TODO: rename to findClassScope
  def findThisScope(scope: Option[Node.Node])(implicit context: ScopeContext): Option[Node.ClassDeclaration] = {
    findDefScope(scope).collect {
      case c: Node.ClassDeclaration => c
    }
  }

  def findThisFunction(scope: Option[Node.Node])(implicit context: ScopeContext): Option[Node.Node] = {
    findDefScope(scope).collect {
      case c@IsFunctionScope() => c
    }
  }

  def findThisClassInWalker(walker: ScopeContext): Option[Node.ClassDeclaration] = {
    //println(walker.stack.map(nodeClassName).mkString(":"))
    walker.parents.reverse.collectFirst {
      case c: Node.ClassDeclaration =>
        //println(s"Found ${c.name.map(_.name)}")
        c
    }
  }

    // ignore function scopes, find a class one
  def findThisClass(walker: ScopeContext): Option[Node.ClassDeclaration] = findThisClassInWalker(walker)

  def superClassSymbolDef(cls: Node.ClassDeclaration)(implicit context: ScopeContext): Option[SymId] = {
    Option(cls.superClass).flatMap(symId)
  }

  def superClass(cls: Node.ClassDeclaration)(implicit context: ScopeContext): Option[SymbolMapId] = {
    //println(s"superClass ${cls.name.get.name}")

    val baseSym = superClassSymbolDef(cls)

    val baseId = baseSym.flatMap(SymbolTypes.id)

    //println(s"  baseSym ${baseSym.map(_.name)} baseId $baseId")

    baseId
  }

  def findSuperClass(context: ScopeContext): Option[SymbolMapId] = {
    implicit val ctx = context
    val thisScope = findThisClass(context)
    thisScope.flatMap(superClass)
  }

  def getClassId(cls: Node.ClassDeclaration)(implicit context: ScopeContext): Option[Int] = {
    val sid = symId(cls.id)
    sid.map(_.sourcePos)
  }


  def getParents(tpe: SymbolMapId)(ctx: ExpressionTypeContext): Seq[SymbolMapId] = {
    ctx.classInfo.listParents(tpe)
  }


  def findInParents(tpe: SymbolMapId, member: String)(ctx: ExpressionTypeContext): Option[SymbolMapId] = {
    ctx.classInfo.classContains(tpe, member)
    /*
    for {
      clazz <- ctx.classes.get(tpe)
      parent@Node.ClassDeclaration(Defined(Node.SymbolName(c)), _, _) <- includeParents(clazz, Seq(clazz))(ctx)
      ... search parent
    } {
      return Some(c)
    }
    None
    */
  }

  val isConstructorProperty: PartialFunction[Node.ClassBodyElement, Node.MethodDefinition] = {
    case m: Node.MethodDefinition if propertyKeyName(m.key) == "constructor" =>
      m
  }

  def findConstructor(c: Node.ClassDeclaration): Option[Node.MethodDefinition] = {
    c.body.body.collectFirst(isConstructorProperty)
  }

  val inlineBodyName = "inline_^"

  def findInlineBody(c: Node.ClassDeclaration): Option[Node.MethodDefinition] = {
    findMethod(c, inlineBodyName)
  }

  def getMethodMethod(m: Node.MethodDefinition): Option[Node.FunctionExpression] = {
    m.value match {
      case x: Node.FunctionExpression =>
        Some(x)
      case _ =>
        None
    }
  }

  def getMethodBody(m: Node.MethodDefinition): Option[Node.BlockStatement] = {
    m.value match {
      case Node.FunctionExpression(id, params, body, generator) =>
        Some(body)
      case _ =>
        None
    }
  }

  def newMethod(k: String, args: Seq[Node.FunctionParameter], methodBody: Node.BlockStatement, tokensFrom: Node.Node, isStatic: Boolean = false): Node.MethodDefinition = {
    Node.MethodDefinition(
      key = Node.Identifier(k).copyLoc(tokensFrom),
      false,
      Node.FunctionExpression(null, args, methodBody.copyLoc(tokensFrom), false).withTokens(tokensFrom),
      if (k == "constructor") "constructor" else "method",
      isStatic
    ).withTokens(tokensFrom)
  }

  def classInlineBody(cls: Node.ClassDeclaration, tokensFrom: Node.Node): Node.MethodDefinition = {
    //println(s"Class inline body $cls")
    val present = findInlineBody(cls)
    val method = present.getOrElse {
      val newInlineBody = newMethod(inlineBodyName, Seq(), Node.BlockStatement(Seq()), tokensFrom)
      cls.body.body = cls.body.body :+ newInlineBody
      newInlineBody
    }
    method
  }



  def findMethod(c: Node.ClassDeclaration, name: String): Option[Node.MethodDefinition] = {
    c.body.body.collectFirst {
      case m: Node.MethodDefinition if propertyKeyName(m.key) == name => m
    }
  }

  def findProperty(c: Node.ClassDeclaration, name: String): Option[Node.MethodDefinition] = {
    c.body.body.collectFirst {
      case m: Node.MethodDefinition if propertyKeyName(m.key) == name => m
    }
  }

  def propertyIsStatic(p: Node.ClassBodyElement): Boolean = {
    p match {
      case p: Node.MethodDefinition =>
        p.static
    }
  }

  def replaceProperty(c: Node.ClassDeclaration, oldP: Node.ClassBodyElement, newP: Node.ClassBodyElement): Node.ClassDeclaration = {
    c.body.body = c.body.body.map(p => if (p == oldP) newP else p)
    c
  }

  def deleteVarMember(c: Node.ClassDeclaration, member: Regex): Node.ClassDeclaration = {
    val inlineBody = Classes.findInlineBody(c)
    inlineBody.fold(c) { ib =>
      // filter member variables as well
      val retIB = ib.cloneNode()
      val body = getMethodBody(retIB)
      for (b <- body) {
        b.body.filterNot {
          case Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(v), _)), _) if member.findFirstIn(v).isDefined =>
            true
          case _ =>
            false
        }
        Classes.replaceProperty(c, ib, retIB)
      }
      c
    }
  }


  def transformClassParameters(c: Node.ClassDeclaration, init: Node.Expression): Node.Expression = {
    val transformed = for {
      cons <- findConstructor(c)
      constructorMethod <- getMethodMethod(cons)
    } yield {
      init.transformAfter { (node, transformer) =>
        implicit val ctx = transformer
        node match {
          case sym@Node.Identifier(name) =>
            val pn = constructorMethod.params.find(parameterName(_)._1.name == name)
            pn.fold(sym) { p =>
              val c = sym.copy().copyLoc(sym)
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
    var classes = Map.empty[SymbolMapId, Node.ClassDeclaration]
    n.top.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case d: Node.ClassDeclaration =>
          for {
            id <- symId(d.id)
          } {
            classes += id -> d
          }
          true
        case _: Node.Program =>
          false
        case _ =>
          false
      }
    }
    classes
  }

  case class ClassListHarmony(classes: Map[SymbolMapId, Node.ClassDeclaration]) {

    def this(n: NodeExtended) = this(classListHarmony(n))

    def get(name: SymbolMapId): Option[Node.ClassDeclaration] = classes.get(name)

    def classPos(name: SymbolMapId): Int = {
      val cls = classes(name)
      cls.body.range._1 // see also classTokenSource
    }

  }

}
