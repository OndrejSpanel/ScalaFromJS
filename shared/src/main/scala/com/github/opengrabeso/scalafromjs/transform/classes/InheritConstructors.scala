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

                // add the constructor call itself, so that type inference binds its parameters and arguments
                val constructorCall = Node.ExpressionStatement(
                  Node.CallExpression(
                    Node.Super(),
                    params.map(p => Node.Identifier(parameterNameString(p)))
                  )
                )

                val body = Node.BlockStatement(Seq(constructorCall))
                val newConstructorValue = Node.FunctionExpression(null, params, body, false, null)

                //println(s"inlineConstructors classInlineBody clone ${accessor.argnames}")
                val newConstructor = Node.MethodDefinition(
                  Node.Identifier("constructor").withTokens(constructor),
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
