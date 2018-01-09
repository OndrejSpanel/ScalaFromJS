package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

import Transform._
import transform._

class ClassTransformTests extends FunSuite with TestUtils {

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

    case class Step(name: Symbol, op: esprima.NodeExtended => esprima.NodeExtended, required: Seq[String], forbidden: Seq[String] = Seq.empty)

    val steps = Seq(
      Step('FillVarMembers, classes.FillVarMembers.apply, Seq("var name = _", "this.name = name", "name:") ),
      Step('InlineConstructors , classes.InlineConstructors.apply, Seq("class Person(name_par:", "this.constructor(name_par)", "name = name_par") ),
      Step('simpleParameters , Parameters.simpleParameters, Seq("class Person(name_par:", "this.constructor(name_par)", "name = name_par") ),
      Step('varInitialization, onTopNode(Variables.varInitialization), Seq("class Person(name_par:", "this.constructor(name_par)", "var name = name_par") ),
      Step('inlineConstructorVars, Parameters.inlineConstructorVars, Seq("class Person(var name:", "this.constructor(name)"), Seq("name_par"))
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
