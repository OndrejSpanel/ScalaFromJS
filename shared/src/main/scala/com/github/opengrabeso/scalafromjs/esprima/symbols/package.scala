package com.github.opengrabeso.scalafromjs.esprima

import com.github.opengrabeso.esprima.Node
import Node._
import com.github.opengrabeso.scalafromjs.Classes

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

package object symbols {
  // create symbol lists for all relevant scopes
  class ScopeInfo(val scopeId: ScopeContext.ScopeId) {
    var symbols = Set.empty[String] // all symbols defined in the scope
  }

  object SymId {
    implicit val ord = Ordering.by(unapply)
  }

  case class SymId(name: String, sourcePos: (Int, Int)) {
    override def toString = s"$name:$sourcePos"
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
  def symId(name: String)(implicit context: ScopeContext): Option[SymId] = Some(context.findSymId(name))
  def symId(name: Node.Identifier)(implicit context: ScopeContext): Option[SymId] = Some(context.findSymId(name.name))

  object Id {
    def unapply(name: String)(implicit context: ScopeContext): Option[SymId] = {
      symId(name)
    }
    def unapply(name: Node.Identifier)(implicit context: ScopeContext): Option[SymId] = {
      symId(name)
    }

    def apply(name: String)(implicit context: ScopeContext): SymId = context.findSymId(name)
    def apply(name: Node.Identifier)(implicit context: ScopeContext): SymId = context.findSymId(name.name)
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

    def findScope(sym: String): Option[(Node.Node, ScopeInfo)] = {
      for {
        i <- scopes.indices.reverse
        scope = scopes(i)
        // class symbols are accessible only using this.xxx notation - never use them to match a plain identifier
        if !scope._1.isInstanceOf[Node.ClassBody]
      } {
        if (scope._2.symbols.contains(sym)) return Some(scope)
      }
      None
    }
    def localSymbols: Set[String] = scopes.last._2.symbols

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
      scopes.reverse.find(s => IsFunctionScope.unapply(s._1))
    }


    def contextUntil(node: Node.Node): ScopeContext = {
      val parentsUntil = parents.take(parents.prefixLength(_ != node) + 1)
      val scopesUntil = scopes.take(scopes.prefixLength(_._1 != node) + 1)

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

    def findSymId(sym: String): SymId = {
      val scope = findScope(sym)
      // when symbol not found in any scope, consider it a global one
      scope.fold(SymId(sym, -1 -> -1))(info => SymId(sym, getNodeId(info._1)))
    }

    def parent(level: Int = 0): Option[Node] = if (level + 1 < parents.length) Some(parents(parents.length - 2 - level)) else None
    def top: Option[Node] = parents.lastOption
    def stack = parents

    def scopeId: ScopeId = getNodeId(scopes.last._1)
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
          val symId = context.findSymId(name)
          builder += symId
          false
        case _ =>
          false
      }

    }
    builder.result()
  }
}
