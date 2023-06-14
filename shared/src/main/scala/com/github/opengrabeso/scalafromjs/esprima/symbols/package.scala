package com.github.opengrabeso.scalafromjs.esprima

import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.esprima.walker
import Node._
import com.github.opengrabeso.scalafromjs.Classes

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

package object symbols {
  // create symbol lists for all relevant scopes
  class ScopeInfo(val scopeId: ScopeContext.ScopeId) {
    /**
      *  all symbols defined in the scope
      *  member functions are marked with true, as their name resolution is different (dot not needed from within a class)
      *
      *  TODO: is this necessary? what other symbols could be defined in a class scope other than member functions?
      *  Value members are not defined as identifiers at all, they are always accessed using Dot
      *  What about properties?
      *  */
    var symbols = Map.empty[String, Boolean]
  }

  object SymId {
    implicit val ord = Ordering.by(unapply)

    def global(name: String): SymId = SymId(name, (-1, -1))
  }

  case class SymId(name: String, sourcePos: (Int, Int)) {
    override def toString = {
      if (isGlobal) {
        s"$name:global"
      } else {
        s"$name:$sourcePos"
      }
    }
    def isGlobal = sourcePos._1 < 0

    def compare(that: SymId) = {
      val d = name compare that.name
      if (d != 0) {
        d
      } else {
        implicitly[Ordering[(Int, Int)]].compare(sourcePos, that.sourcePos)
      }
    }
  }

  // TODO: refactor: simplify - SymId is always known
  def symId(name: String)(implicit context: ScopeContext): Option[SymId] = Some(context.findSymId(name, false))
  def symId(name: Node.Identifier)(implicit context: ScopeContext): Option[SymId] = Some(context.findSymId(name.name, false))

  def memberFunId(name: String)(implicit context: ScopeContext): Option[SymId] = Some(context.findSymId(name, true))
  def memberFunId(name: Node.Identifier)(implicit context: ScopeContext): Option[SymId] = Some(context.findSymId(name.name, true))

  object Id {
    def unapply(name: String)(implicit context: ScopeContext): Option[SymId] = {
      symId(name)
    }
    def unapply(name: Node.Identifier)(implicit context: ScopeContext): Option[SymId] = {
      symId(name)
    }

    def apply(name: String)(implicit context: ScopeContext): SymId = context.findSymId(name, false)
    def apply(name: Node.Identifier)(implicit context: ScopeContext): SymId = context.findSymId(name.name, false)
  }

  object MemberFunId {
    def unapply(name: String)(implicit context: ScopeContext): Option[SymId] = {
      memberFunId(name)
    }
    def unapply(name: Node.Identifier)(implicit context: ScopeContext): Option[SymId] = {
      memberFunId(name)
    }

    def apply(name: String)(implicit context: ScopeContext): SymId = context.findSymId(name, true)
    def apply(name: Node.Identifier)(implicit context: ScopeContext): SymId = context.findSymId(name.name, true)
  }

  object ScopeContext {
    type ScopeId = (Int, Int)
    def markAsClass(r: ScopeId): ScopeId = r.copy(_2 = r._2 + 1)

    def getNodeId(n: Node.Node): ScopeId = {
      assert(IsScope.unapply(n))
      if (n.range != null) {
        n match {
          case cb: ClassBody =>
            markAsClass(n.range)
          case fe: FunctionExpression if fe.id != null && fe.id.name == Classes.inlineBodyName =>
            markAsClass(n.range)
          case _ =>
            n.range
        }
      }
      else {
        throw new NoSuchElementException(s"Missing node id for $n")
      }
    }
    case class EnterScopeValue(isScope: Boolean)
  }

  class ScopeContext {
    import ScopeContext._

    def enterScope(node: Node): EnterScopeValue = {
      // we should never enter a scope twice, this means somebody already entered it and we are confused
      assert(!parents.exists(node eq _))

      parents.push(node)
      val isScope = IsDeclScope.unapply(node)(this)
      if (isScope) {
        scopes.push(node -> new ScopeInfo(getNodeId(node)))
      }
      scanSymbols(node)
      EnterScopeValue(isScope)
    }

    def scanSymbols(node: Node) = {
      // check on symbol collections for easier symbol declaration debugging
      val symbols = SymbolDeclaration.declaredSymbols(node)
      if (scopes.nonEmpty && symbols.nonEmpty) {
        scopes.last._2.symbols ++= symbols
      }
    }

    def leaveScope(entered: EnterScopeValue) = {
      parents.pop()

      if (entered.isScope) {
        scopes.pop()
      }
    }

    def withScope[T](node: Node*)(callback: => T) = {
      val s = node.map(enterScope)
      val ret = callback
      s.reverse.foreach(leaveScope)
      ret
    }


    val parents = ArrayBuffer.empty[Node.Node]
    val scopes =  ArrayBuffer.empty[(Node.Node, ScopeInfo)]

    /**
      * When searching for a member call, we use the class scopes as well. Otherwise we do not,
      * as data members can only be access using dot notation, never as a plain identifier
      * */
    def findScope(sym: String, memberCall: Boolean): Option[(Node.Node, ScopeInfo)] = {
      scopes.findLast { case (node, scope) =>
        scope.symbols.get(sym) match {
          case Some(memberFun) =>
            if (!node.isInstanceOf[Node.ClassBody]) {
              // when it is a normal scope, it matches everything, call or not
              true
            } else {
              // class scope matching only when the symbol is used when a member is expected
              memberCall //&& memberFun
            }
          case None =>
            false
        }
      }
    }

    def localSymbols: Set[String] = scopes.last._2.symbols.keySet

    def findTypedParent[T<: Node: ClassTag]: Option[T] = {
      parents.reverse.collectFirst {
        case c: T => c
      }
    }

    def findClassScope: Option[Node] = {
      parents.reverse.collectFirst {
        case c: Node.ClassDeclaration => c
        case o: Node.ObjectExpression => o
      }
    }

    // find the first parent scope which is a function (member, explicit, implicit, arrow ..._)
    def findFuncScope: Option[(Node.Node, ScopeInfo)] = {
      scopes.findLast(s => IsFunctionScope.unapply(s._1))
    }


    def contextUntil(node: Node.Node): ScopeContext = {
      val parentsUntil = parents.take(parents.segmentLength(_ != node) + 1)
      val scopesUntil = scopes.take(scopes.segmentLength(_._1 != node) + 1)

      val ret = new ScopeContext
      ret.parents ++= parentsUntil
      ret.scopes ++= scopesUntil
      ret
    }

    def findScopeById(scopeId: ScopeId): Option[(Node.Node, ScopeContext)] = {
      for (i <- scopes.indices.reverse) {
        if (scopes(i)._2.scopeId == scopeId) {
          return Some(scopes(i)._1, contextUntil(scopes(i)._1))
        }
      }
      None
    }

    def findSymId(sym: String, memberCall: Boolean): SymId = {
      val scope = findScope(sym, memberCall)
      // when symbol not found in any scope, consider it a global one
      scope.fold {
        SymId(sym, -1 -> -1)
      }(info => SymId(sym, getNodeId(info._1)))
    }

    def parent(level: Int = 0): Option[Node] = if (level + 1 < parents.length) Some(parents(parents.length - 2 - level)) else None
    def top: Option[Node] = parents.lastOption
    def stack = parents

    def scopeId: ScopeId = getNodeId(scopes.last._1)

    def scopeDelimiter = "/" // we do not want to use dot as dot is hard to match with regex

    def scopeFromExpression(node: Node.Node): Option[String] = {
      node match {
        case ThisExpression() =>
          None
        case Node.Identifier(name) =>
          Some(name)
        case expr Dot name =>
          scopeFromExpression(expr) match {
            case Some(scope) =>
              Some(scope + scopeDelimiter + name)
            case None =>
              Some(name)
          }
        case _ =>
          None
      }
    }
    def scopePath: String = parents.flatMap {
      case cls: Node.ClassDeclaration =>
        Some(cls.id.name)
      case prop: Node.Property =>
        val name = propertyName(prop)
        Some(name)
      // handle assignment in mostly the same way as you would handle variable declaration
      case Node.AssignmentExpression("=", expr, _) =>
        scopeFromExpression(expr)
      case Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(name), _, _)), _) =>
        Some(name)
      case Node.FunctionDeclaration(Node.Identifier(id), _, _, _, _) =>
        Some(id)
      /* it would be possible to not consider methods as a scope, this would makes identifying member variables easier
      Having the method scope would helps for local variables, though, and you can skip it using .*
       */
      case Node.MethodDefinition(Node.Identifier(id), _, _, _, _, _) =>
        Some(id)
      case _ =>
        None
    }.mkString(scopeDelimiter)

  }

  /**
    * Walk while tracking a scope stack and a symbol information
    * */
  def doWalk(ast: Node, context: ScopeContext)(callback: (Node, ScopeContext) => Boolean, after: (Node, ScopeContext) => Boolean): Unit = {
    val origScopes = context.scopes.length
    val origParents = context.parents.length

    if (ast != null) {
      if (callback == null || !callback(ast, context)) {
        walker.walkInto(ast) { node =>
          context.withScope(node)(walk(node, context)(callback, after))
        }
      }
      if (after != null) {
        after(ast, context)
      }
    }

    assert(context.scopes.length == origScopes)
    assert(context.parents.length == origParents)
  }

  def walk(ast: Node, context: ScopeContext = new ScopeContext)(callback: (Node, ScopeContext) => Boolean = null, after: (Node, ScopeContext) => Boolean = null): Unit = {
    if (context.parents.isEmpty) {
      context.withScope(ast) {
        doWalk(ast, context)(callback, after)
      }
    } else {
      doWalk(ast, context)(callback, after)
    }
  }


  def listAllSymbols(node: Node): Set[SymId] = {
    val builder = Set.newBuilder[SymId]
    walk(node) { (node, context) =>
      node match {
        case Identifier(name) =>
          val symId = context.findSymId(name, false)
          builder += symId
          false
        case _ =>
          false
      }

    }
    builder.result()
  }
}
