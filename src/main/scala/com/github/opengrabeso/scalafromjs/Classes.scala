package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import JsUtils._
import SymbolTypes._
import com.github.opengrabeso.scalafromjs.Transform.ExpressionTypeContext
import com.github.opengrabeso.scalafromjs.esprima.symbols._

import scala.util.matching.Regex

object Classes {

  def findThisClassInWalker(walker: ScopeContext): Option[Node.ClassDeclaration] = {
    //println(walker.stack.map(nodeClassName).mkString(":"))
    walker.findTypedParent[Node.ClassDeclaration]
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

  def getClassId(cls: Node.ClassDeclaration)(implicit context: ScopeContext): Option[ScopeContext.ScopeId] = {
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
    case m: Node.MethodDefinition if m.key != null && propertyKeyName(m.key) == "constructor" =>
      m
  }

  def findConstructor(c: Node.ClassDeclaration): Option[Node.MethodDefinition] = {
    Option(c.body).flatMap(_.body.collectFirst(isConstructorProperty))
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
      case Node.FunctionExpression(id, params, body, generator, _) =>
        Some(body)
      case _ =>
        None
    }
  }

  def newMethod(k: String, args: Seq[Node.FunctionParameter], methodBody: Node.BlockStatement, tokensFrom: Node.Node, isStatic: Boolean = false): Node.MethodDefinition = {
    Node.MethodDefinition(
      key = Node.Identifier(k).copyLoc(tokensFrom),
      null,
      false,
      Node.FunctionExpression(null, args, methodBody.copyLoc(tokensFrom), false, null).withTokens(tokensFrom),
      if (k == "constructor") "constructor" else "method",
      isStatic
    ).withTokens(tokensFrom)
  }

  def newProperty(kind: String, k: String, args: Seq[Node.FunctionParameter], methodBody: Node.BlockStatement, tokensFrom: Node.Node, isStatic: Boolean = false): Node.Property = {
    Node.Property(
      kind = kind,
      key = Node.Identifier(k).copyLoc(tokensFrom),
      false,
      Node.FunctionExpression(null, args, methodBody.copyLoc(tokensFrom), false, null).withTokens(tokensFrom),
      false,
      false
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



  def findMethod(c: Node.ClassDeclaration, name: String, static: Boolean = false): Option[Node.MethodDefinition] = {
    Option(c.body).flatMap(_.body.collectFirst {
      case m: Node.MethodDefinition if m.key != null && propertyKeyName(m.key) == name && m.static == static =>
        m
    })
  }

  def findObjectMethod(c: Node.ObjectExpression, name: String, static: Boolean = false): Option[Node.PropertyValue] = {
    c.properties.collectFirst {
      case Node.Property(_, KeyName(`name`), _, m, _, _) =>
        m
    }
  }

  def findProperty(c: Node.ClassDeclaration, name: String): Option[Node.MethodDefinition] = {
    Option(c.body).flatMap(_.body.collectFirst {
      case m: Node.MethodDefinition if m.key != null && propertyKeyName(m.key) == name => m
    })
  }

  def propertyIsStatic(p: Node.ClassBodyElement): Boolean = {
    p match {
      case p: ScalaNode.MemberTemplate =>
        false
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
        b.body = b.body.filterNot {
          case VarDecl(v, _, _) if member.findFirstIn(v).isDefined =>
            true
          case _ =>
            false
        }
        Classes.replaceProperty(c, ib, retIB)
      }
      c.body.body = c.body.body.filterNot {
        case Node.MethodDefinition(Node.Identifier(v), _, _, _, _, _) if member.findFirstIn(v).isDefined =>
          true
        case _ =>
          false
      }

      c
    }
  }


  def transformClassParameters(c: Node.ClassDeclaration, init: Node.Expression)(implicit ctx: ScopeContext): Node.Expression = {
    val transformed = for {
      cons <- findConstructor(c)
      constructorMethod <- getMethodMethod(cons)
    } yield {
      //val parMap = (constructorMethod.params.map(parameterNameString) zip inlineBodyMethod.params.map(parameterNameString)).toMap
      val params = constructorMethod.params.map(parameterNameString)

      val matchIdentifier: PartialFunction[Node.Node, Node.Node] = {
        case sym@Node.Identifier(name) if params contains name =>
          // TODO: verify parameter is not hidden by another variable
          // use corresponding parameter name from the class constructor (inline body)
          //val pn = parMap.getOrElse(name, name)
          //println(s"transformClassParameters ${c.name}")
          Dot(Node.ThisExpression(), sym).withTokens(sym)
      }


      init.transformBefore(ctx) { (node, descend, transformer) =>
        implicit val ctx = transformer
        val composed = matchIdentifier orElse[Node.Node, Node.Node] {
          case p: Node.Property =>
            // do not transform property names
            p.value = (matchIdentifier orElse[Node.Node, Node.Node] {
              case n =>
                descend(n, transformer)
                n
            })(p.value).asInstanceOf[Node.PropertyValue]
            p

          case _ =>
            descend(node, transformer)
            node
        }
        composed(node)
      }
    }
    transformed.getOrElse(init)
  }


  /*
  classes: mapping from a symbol id to a tuple of parent and declaration
  */
  case class ClassListHarmony(classes: Map[SymbolMapId, (Option[SymId], Node.ClassDeclaration)], anonymous: Map[SymbolMapId, Node.ObjectExpression]) {

    def get(name: SymbolMapId): Option[Node.ClassDeclaration] = classes.get(name).map(_._2)
    def getParent(name: SymbolMapId): Option[SymId] = classes.get(name).flatMap(_._1)
    def getAnonymous(name: SymbolMapId): Option[Node.ObjectExpression] = anonymous.get(name)

    def classPos(name: SymbolMapId): (Int, Int) = {
      classes.get(name).map(_._2.body.range).getOrElse((-1, -1))
    }

  }

  object ClassListHarmony {
    def empty: ClassListHarmony = new ClassListHarmony(Map.empty, Map.empty)
    def fromAST(n: Node.Program, innerClasses: Boolean = true): ClassListHarmony = {
      var classes = Map.empty[SymbolMapId, (Option[SymId], Node.ClassDeclaration)]
      var anonymous = Map.empty[SymbolMapId, Node.ObjectExpression]
      n.walkWithScope { (node, context) =>
        implicit val ctx = context
        node match {
          case d: Node.ClassDeclaration =>
            for {
              id <- symId(d.id)
            } {
              val parentId = Option(d.superClass).flatMap(symId)
              classes += id -> (parentId, d)
            }
            !innerClasses // classes may contain inner classes - when the caller wants them, continue the traversal
          case oe: Node.ObjectExpression =>
            val id = symbols.ScopeContext.getNodeId(oe)
            anonymous += SymbolMapId("<anonymous>", id) -> oe
            !innerClasses

          case _: Node.Program =>
            false
          case _ =>
            false
        }
      }
      new ClassListHarmony(classes, anonymous)
    }
  }
}
