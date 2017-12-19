package com.github.opengrabeso.scalafromjs.esprima

import com.github.opengrabeso.esprima.Node
import Node._

import scala.collection.mutable.ArrayBuffer

package object symbols {
  // create symbol lists for all relevant scopes
  class ScopeInfo {
    var symbols = Set.empty[String] // all symbols defined in the scope
  }

  case class SymId(name: String, sourcePos: Int) {
    override def toString = s"$name:$sourcePos"
    def compare(that: SymId) = {
      val d = name compare that.name
      if (d != 0) {
        d
      } else {
        sourcePos - that.sourcePos
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
    def getNodeId(n: Node.Node) = {
      if (n.range != null) n.range._1
      else ??? // System.identityHashCode(n)
    }
    case class EnterScopeValue(isScope: Boolean)
  }

  class ScopeContext {
    import ScopeContext._

    def enterScope(node: Node): EnterScopeValue = {
      parents.push(node)
      val isScope = IsDeclScope.unapply(node)(this)
      if (isScope) {
        scopes.push(node -> new ScopeInfo)
      }
      scanSymbols(node)
      EnterScopeValue(isScope)
    }

    def scanSymbols(node: Node) = {
      if (scopes.length >= 2) {
        val parentScope = scopes(scopes.length - 2)
        parentScope._2.symbols ++= SymbolDeclaration.declaredSymbolsExtern(node)
      }
      if (scopes.nonEmpty) {
        scopes.last._2.symbols ++= SymbolDeclaration.declaredSymbols(node)
      }
    }

    def leaveScope(node: Node, entered: EnterScopeValue) = {
      parents.pop()
      if (entered.isScope) {
        scopes.pop()
      }
    }

    val parents = ArrayBuffer.empty[Node.Node]
    val scopes =  ArrayBuffer.empty[(Node.Node, ScopeInfo)]

    def findScope(sym: String): Option[(Node.Node, ScopeInfo)] = {
      for (i <- scopes.indices.reverse) {
        if (scopes(i)._2.symbols.contains(sym)) return Some(scopes(i))
      }
      None
    }

    def contextUntil(node: Node.Node): ScopeContext = {
      val parentsUntil = parents.takeWhile(_ != node)
      val scopesUntil = scopes.takeWhile(_._1 != node)

      val ret = new ScopeContext
      ret.parents ++= parentsUntil
      ret.scopes ++= scopesUntil
      ret
    }

    def findScopeById(scopeId: Int): Option[(Node.Node, ScopeContext)] = {
      for (i <- scopes.indices.reverse) {
        if (getNodeId(scopes(i)._1) == scopeId) {
          return Some(scopes(i)._1, contextUntil(scopes(i)._1))
        }
      }
      None
    }

    def findSymId(sym: String): SymId = {
      val scope = findScope(sym)
      // when symbol not found in any scope, consider it a global one
      scope.fold(SymId(sym, -1))(info => SymId(sym, getNodeId(info._1)))
    }

    def parent(level: Int = 0): Option[Node] = if (level + 1 < parents.length) Some(parents(parents.length - 2 - level)) else None
    def stack = parents

    def scopeId: Int = getNodeId(scopes.last._1)
  }

  /**
    * Walk while tracking a scope stack and a symbol information
    * */
  def walk(ast: Node, context: ScopeContext = new ScopeContext)(callback: (Node, ScopeContext) => Boolean): Unit = {
    val origScopes = context.scopes.length
    val origParents = context.parents.length

    if (ast != null) {
      val es = context.enterScope(ast)
      if (!callback(ast, context)) {
        walker.walkInto(ast)(node => walk(node, context)(callback))
      }
      context.leaveScope(ast, es)
    }

    assert(context.scopes.length == origScopes)
    assert(context.parents.length == origParents)
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
