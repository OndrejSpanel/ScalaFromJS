package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.esprima.Node._
import com.github.opengrabeso.esprima.{Esprima, Parser}
import com.github.opengrabeso.scalafromjs.esprima.symbols.ScopeContext

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
      symbols.walk(ast)(callback)
    }

    def walkWithScopeAfter(scope: ScopeContext)(after: (Node, ScopeContext) => Boolean) = {
      symbols.walk(ast, scope)(after = after)
    }

    def walkWithScopeAfter(after: (Node, ScopeContext) => Boolean) = {
      symbols.walk(ast, new ScopeContext)(after = after)
    }



    /*
    * Used after each transform to detect any missing scope loc information early
    * */
    def verifyScopesValid(): T = {
      // walking through whole AST creates all scopes, this way we verify their loc is setup correctly
      //symbols.walk(ast, new ScopeContext)((_, _) => false)
      if (ast.range == null) {
        // if root has no location data, it is parsed without them and it has no sense to require them
      } else {

        // verify function parameters are still correct
        walk {
          case n@AnyFun(params, _) =>
            params.foreach { p =>
              if (!p.isInstanceOf[FunctionParameter]) {
                println(s"Bad parameter node $p in $n")
                assert(false)
              }
            }
            false
          case vd: VariableDeclarator =>
            if (!vd.id.isInstanceOf[BindingIdentifierOrPattern]) {
              println(s"Bad VariableDeclarator node ${vd.id} in $vd")
              assert(false)
            }
            false
          case _ =>
            false
        }

        // this is a bit longer, but it makes locating issue easier
        def needsLoc(n: Node) = {
          if (n.range == null) {
            println(s"Null range in $n")
          }
          assert(n.range != null)
        }

        walk {
          case n@IsScope() =>
            needsLoc(n)
            false
          case n: ForStatement =>
            needsLoc(n)
            false
          case n: VariableDeclaration =>
            needsLoc(n)
            false
          case id: Identifier =>
            needsLoc(id)
            false

          case _ =>
            false
        }
      }
      ast
    }

    /*
    def walkWithDescend(callback: (Node, (Node, ScopeContext) => Unit, ScopeContext) => Boolean) = {
      symbols.walk(ast)(callback)
    }
    */

    def transform(transformer: TreeTransformer): T = {
      import walker._
      if (ast != null) {
        val before = transformer.before(ast, { (node, transformer) =>
          transformInto(node) { node =>
            transformer.context.withScope(node)(node.transform(transformer))
          }
          node
        })
        if (before != null) {
          before.asInstanceOf[T]
        } else {
          transformInto(ast) { node =>
            transformer.context.withScope(node)(node.transform(transformer))
          }
          val after = transformer.after(ast)
          after.asInstanceOf[T]
        }
      } else {
        ast
      }
    }

    def transformBefore(ctx: ScopeContext)(_before: (Node, (Node, TreeTransformer) => Node, TreeTransformer) => Node): T = {
      val origScopes = ctx.scopes.length
      val origParents = ctx.parents.length
      object tr extends TreeTransformer {
        override val context = ctx

        override def before(node: Node, descend: (Node, TreeTransformer) => Node) = _before(node, descend, tr)
      }
      val ret = ast.transform(tr).verifyScopesValid()

      assert(ctx.scopes.length == origScopes)
      assert(ctx.parents.length == origParents)

      ret
    }

    def transformAfter(ctx: ScopeContext)(_after: (Node, TreeTransformer) => Node): T = {
      val origScopes = ctx.scopes.length
      val origParents = ctx.parents.length
      object tr extends TreeTransformer {
        override val context = ctx

        override def after(node: Node) = _after(node, tr)
      }
      ast.verifyScopesValid()
      val ret = ast.transform(tr).verifyScopesValid()

      assert(ctx.scopes.length == origScopes)
      assert(ctx.parents.length == origParents)

      ret
    }

    def transformAfterSimple(ctx: ScopeContext)(_after: Node => Node): T = {
      object tr extends TreeTransformer {
        override val context = ctx

        override def after(node: Node) = _after(node)
      }
      ast.transform(tr).verifyScopesValid()
    }

    def transformBefore(_before: (Node, (Node, TreeTransformer) => Node, TreeTransformer) => Node): T = {
      val ctx = new ScopeContext()
      ctx.withScope(ast) {
        transformBefore(ctx)(_before)
      }
    }

    def transformAfter(_after: (Node, TreeTransformer) => Node): T = {
      val ctx = new ScopeContext()
      ctx.withScope(ast) {
        transformAfter(ctx)(_after)
      }
    }

    def transformAfterSimple(_after: Node => Node): T = {
      val ctx = new ScopeContext()
      ctx.withScope(ast) {
        transformAfterSimple(ctx)(_after)
      }
    }

    def cloneDeep(): T = {
      transformAfterSimple(_.cloneNode())
    }

  }

  object ParseOptions extends Parser.Options {
    range = true
    attachComment = true
    sourceType = "module" // to avoid "Unexpected token" with import statements - see https://github.com/jquery/esprima/issues/1273
  }
  def parse(code: String): Program = {
    Esprima.parse(code, ParseOptions)
  }

}
