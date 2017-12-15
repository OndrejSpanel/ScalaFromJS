package com.github.opengrabeso

import _root_.esprima.Node._
import _root_.esprima.{Esprima, Parser}
import com.github.opengrabeso.esprima.symbols.ScopeContext

package object esprima extends NodeExt {
  // interface inspired by uglify-js
  trait TreeTransformer {

    implicit val context: ScopeContext
    def before(node: Node, descend: (Node, TreeTransformer) => Node): Node = null

    def after(node: Node): Node = null

    def parent(level: Int = 0) = context.parent(level)
    def parentCount: Int = context.parents.length - 1
  }

  implicit class ASTOps[T <: Node](ast: T) {
    def walk(callback: Node => Boolean) = {
      walker.walkRecursive(ast)(callback)()
    }

    def walkWithScope(scope: ScopeContext)(callback: (Node, ScopeContext) => Boolean) = {
      symbols.walk(ast, scope)(callback)
    }

    def walkWithScope(callback: (Node, ScopeContext) => Boolean) = {
      symbols.walk(ast, new ScopeContext)(callback)
    }

    /*
    def walkWithDescend(callback: (Node, (Node, ScopeContext) => Unit, ScopeContext) => Boolean) = {
      symbols.walk(ast)(callback)
    }
    */

    def transform(transformer: TreeTransformer): T = {
      import walker._
      if (ast != null) {
        transformer.context.enterScope(ast)
        val before = transformer.before(ast, { (node, transformer) =>
          transformInto(node)(node => node.transform(transformer))
          node
        })
        transformer.context.leaveScope(ast)
        if (before != null) {
          before.asInstanceOf[T]
        } else {
          val cloned = ast.clone
          transformer.context.enterScope(ast)
          transformInto(cloned)(node => node.transform(transformer))
          val after = transformer.after(cloned)
          transformer.context.leaveScope(ast)
          after.asInstanceOf[T]
        }
      } else {
        ast
      }
    }

    def transformBefore(ctx: ScopeContext)(_before: (Node, (Node, TreeTransformer) => Node, TreeTransformer) => Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer {
        override val context = ctx

        override def before(node: Node, descend: (Node, TreeTransformer) => Node) = _before(node, descend, tr)
      }
      ast.transform(tr)
    }

    def transformAfter(ctx: ScopeContext)(_after: (Node, TreeTransformer) => Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer {
        override val context = ctx

        override def after(node: Node) = _after(node, tr)
      }
      ast.transform(tr)
    }

    def transformAfterSimple(ctx: ScopeContext)(_after: Node => Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer {
        override val context = ctx

        override def after(node: Node) = _after(node)
      }
      ast.transform(tr)
    }

    def transformBefore(_before: (Node, (Node, TreeTransformer) => Node, TreeTransformer) => Node): T = {
      transformBefore(new ScopeContext)(_before)
    }

    def transformAfter(_after: (Node, TreeTransformer) => Node): T = {
      transformAfter(new ScopeContext)(_after)
    }

    def transformAfterSimple(_after: Node => Node): T = {
      transformAfterSimple(new ScopeContext)(_after)
    }
  }

  object ParseOptions extends Parser.Options {
    range = true
    attachComment = true
  }
  def parse(code: String): Program = {
    Esprima.parse(code, ParseOptions)
  }

}
