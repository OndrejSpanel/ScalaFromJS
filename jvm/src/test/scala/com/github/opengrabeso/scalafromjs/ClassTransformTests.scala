package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

import Transform._
import transform._

class ClassTransformTests extends AnyFunSuite with TestUtils {

  implicit class Check(f: String) {
    def required(str: String*) = {
      str.foreach(s => assert(f.contains(s)))
    }

    def forbidden(str: String*) = {
      str.foreach(s => assert(!f.contains(s)))
    }
  }

  implicit class AST(ast: esprima.NodeExtended)(implicit input: String) {
    def asString: String = ScalaOut.output(ast, input).mkString
  }

  test("Individual class transforms should produce expected results") {
    implicit val input: String = """
      class Person {
          constructor(name) {
              this.name = name;
              var x
          }

      }

      var bob = new Person("Bob")
      """

    case class Step(name: String, op: esprima.NodeExtended => esprima.NodeExtended, required: List[String], forbidden: List[String] = Nil)

    val steps = Seq(
      Step("FillVarMembers", classes.FillVarMembers.apply, List("var name = _", "this.name = name", "name:") ),
      Step("InlineConstructors" , classes.InlineConstructors.apply, List("class Person(name_par:", "this.constructor(name_par)", "name = name_par") ),
      Step("simpleParameters" , Parameters.simpleParameters, List("class Person(name_par:", "this.constructor(name_par)", "name = name_par") ),
      Step("varInitialization", onTopNode(Variables.varInitialization), List("class Person(name_par:", "this.constructor(name_par)", "var name = name_par") ),
      Step("inlineConstructorVars", Parameters.inlineConstructorVars, List("class Person(var name:", "this.constructor(name)"), List("name_par"))
    )

    val ast = esprima.parse(input)
    val ext = esprima.NodeExtended(ast)
    val results = steps.scanLeft(ext.asString -> ext) { case ((_, e), op) =>
      val ret = op.op(e)
      val retAsString = ret.asString
      println(s"Step ${op.name}")
      println(retAsString)
      retAsString.required(op.required:_*)
      retAsString.forbidden(op.forbidden:_*)
      retAsString -> ret
    }
    results.last._1
  }
}
