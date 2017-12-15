package com.github.opengrabeso

import _root_.esprima.Node._
import _root_.esprima.{Esprima, Parser}
import com.github.opengrabeso.esprima.symbols.ScopeContext

package object esprima extends NodeExt {
  // interface inspired by uglify-js
  trait TreeTransformer extends ScopeContext {
    def before(node: Node, descend: (Node, TreeTransformer) => Node): Node = null

    def after(node: Node): Node = null
  }

  implicit class ASTOps[T <: Node](ast: T) {
    def walk(callback: Node => Boolean) = {
      walker.walkRecursive(ast)(callback)()
    }

    def walkWithScope(callback: (Node, ScopeContext) => Boolean) = {
      symbols.walk(ast)(callback)
    }

    /*
    def walkWithDescend(callback: (Node, (Node, ScopeContext) => Unit, ScopeContext) => Boolean) = {
      symbols.walk(ast)(callback)
    }
    */

    def transform(transformer: TreeTransformer): T = {
      import walker._
      if (ast != null) {
        val before = transformer.before(ast, {(node, transformer) =>
          transformer.enterScope(ast)
          transformInto(node)(node => node.transform(transformer) )
          transformer.leaveScope(ast)
          node
        })
        if (before != null) {
          before.asInstanceOf[T]
        } else {
          val cloned = ast.clone
          transformer.enterScope(ast)
          transformInto(cloned)(node => node.transform(transformer) )
          transformer.leaveScope(ast)
          val after = transformer.after(cloned)
          after.asInstanceOf[T]
        }
      } else {
        ast
      }
    }

    def transformBefore(_before: (Node, (Node, TreeTransformer) => Node, TreeTransformer) => Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer {
        override def before(node: Node, descend: (Node, TreeTransformer) => Node) = _before(node, descend, tr)
      }
      ast.transform(tr)
    }

    def transformAfter(_after: (Node, TreeTransformer) => Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer {
        override def after(node: Node) = _after(node, tr)
      }
      ast.transform(tr)
    }

    def transformAfterSimple(_after: Node => Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer {
        override def after(node: Node) = _after(node)
      }
      ast.transform(tr)
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
