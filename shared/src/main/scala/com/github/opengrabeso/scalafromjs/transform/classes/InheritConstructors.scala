package com.github.opengrabeso.scalafromjs
package transform
package classes

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import Expressions._
import Variables._
import Symbols._
import SymbolTypes._
import VariableUtils._
import com.github.opengrabeso.scalafromjs
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId}

import scala.collection.mutable
import scala.collection.Seq

/*
Inherit a class constructor when it is missing

Example:

from:

      class Person {
          constructor(name) {
              this.name = name;
          }
      }

      class Man extends Person {
      }

to:

      class Person {
          constructor(name) {
              this.name = name;
          }
      }

      class Man extends Person {
          constructor(name) {
              super(name)
          }

      }

*/

object InheritConstructors {

  def apply(n: NodeExtended): NodeExtended = {

    val classes = ClassListHarmony.fromAST(n.top)

    val r = n.top.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cls: Node.ClassDeclaration =>

          ctx.withScope(cls.body) {

            val clsTokenDef = classTokenSource(cls)

            if (findConstructor(cls).isEmpty && cls.superClass != null) {
              // find the superclass and access its constructor
              for {
                Node.Identifier(Id(baseId)) <- Option(cls.superClass)
                c <- classes.get(baseId)
                constructor <- Classes.findConstructor(c)
                AnyFun(params, _) <- Option(constructor.value)
              } {
                //

                // Constructor parameters originate in the base class, which may be in another
                // source file. Clone them before rebasing the complete synthetic constructor so
                // output splitting cannot follow a nested default value into the base file.
                val inheritedParams = params.map { parameter =>
                  val cloned = parameter.cloneDeep()

                  def rebaseLocation(node: Node.Node): Unit = {
                    node.range = clsTokenDef.range
                    node.loc = clsTokenDef.loc
                    // Comments belong to the base declaration and must not be duplicated by the
                    // synthetic constructor in the derived class.
                    node.leadingComments = null
                    node.innerComments = null
                    node.trailingComments = null
                  }

                  rebaseLocation(cloned)
                  cloned.walk { node =>
                    rebaseLocation(node)
                    false
                  }
                  cloned
                }

                // add the constructor call itself, so that type inference binds its parameters and arguments
                val constructorCall = Node.ExpressionStatement(
                  Node.CallExpression(
                    Node.Super(),
                    inheritedParams.map(p => Node.Identifier(parameterNameString(p)))
                  )
                )

                val body = Node.BlockStatement(Seq(constructorCall))
                val newConstructorValue = Node.FunctionExpression(null, inheritedParams, body, false, null)

                //println(s"inlineConstructors classInlineBody clone ${accessor.argnames}")
                val newConstructor = Node.MethodDefinition(
                  Node.Identifier("constructor"),
                  null, false, newConstructorValue, "constructor", false
                ).withTokensDeep(clsTokenDef)

                cls.body.body = newConstructor +: cls.body.body
              }

            }

            cls
          }
        case _ =>
          node
      }
    }

    n.copy(top = r)
  }

}
