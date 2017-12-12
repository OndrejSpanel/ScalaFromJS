package com.github.opengrabeso

import _root_.esprima.Node.Node

package object esprima extends NodeExt {
  // interface inspired by uglify-js
  trait TreeTransformer {
    def before(node: Node, descend: (Node, TreeTransformer) => Node): Node = null

    def after(node: Node): Node = null
  }

  implicit class ASTOps[T <: Node](ast: T) {
    def walk(callback: Node => Boolean) = {
      walker.walkRecursive(ast)(callback)()
    }

    def transform(transformer: TreeTransformer): T = {
      import walker._
      if (ast != null) {
        val before = transformer.before(ast, _ transform _)
        if (before != null) {
          before.asInstanceOf[T]
        } else {
          val cloned = ast.clone
          transformInto(cloned)(node => transformer.after(node).transform(transformer) )
          cloned.asInstanceOf[T]
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


}
