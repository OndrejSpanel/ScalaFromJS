package com.github.opengrabeso.esprima

import _root_.esprima.Node
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

  class ScopeContext {
    val parents = ArrayBuffer.empty[Node.Node]
    val scopes =  ArrayBuffer.empty[(Node.Node, ScopeInfo)]

    def getNodeId(n: Node.Node) = {
      if (n.range != null) n.range._1
      else System.identityHashCode(n)
    }
    def findSymId(sym: String): SymId = {
      for (i <- scopes.indices.reverse) {
        if (scopes(i)._2.symbols.contains(sym)) return SymId(sym, getNodeId(scopes(i)._1))
      }
      // symbol not found in any scope, consider it a global one
      SymId(sym, -1)
    }
  }

  /**
    * Walk while tracking a scope stack and a symbol information
    * */
  def walk(node: Node)(callback: (Node, ScopeContext) => Boolean): Unit = {
    val context = new ScopeContext

    def callbackWithPrefix(node: Node): Boolean = {
      val ret = callback(node, context)
      if (!ret) {
        val isScope = node.isInstanceOf[IsScope]
        if (isScope) {
          context.scopes.push(node -> new ScopeInfo)
        }
        context.parents.push(node)
      }
      // scan for nodes defining symbols
      node match {
        case Node.SymbolDeclaration(id@_*) =>
          context.scopes.last._2.symbols ++= id
        case _ =>
      }
      ret
    }

    def post(node: Node) = {
      val isScope = node.isInstanceOf[IsScope]
      context.parents.pop()
      if (isScope) {
        context.scopes.pop()
      }
    }

    walker.walkRecursive(node)(callbackWithPrefix)(post)

    assert(context.scopes.isEmpty)
    assert(context.parents.isEmpty)
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
