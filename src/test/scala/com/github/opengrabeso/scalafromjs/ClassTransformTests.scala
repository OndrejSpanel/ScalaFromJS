package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

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
    val ast = esprima.parse(input)

    val ext = esprima.NodeExtended(ast)
    val inputAsString = ext.asString

    inputAsString.required("this.name = name", "name:")

    val vars = transform.classes.FillVarMembers(ext)
    val varsAsString = vars.asString

    varsAsString.required("var name = _", "this.name = name", "name:")

    val ret = transform.classes.InlineConstructors(vars)
    val retAsString = ret.asString

    retAsString.required("class Person(name_par:", "this.constructor(name_par)", "name = name_par")
  }
}
