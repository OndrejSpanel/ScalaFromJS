package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite
import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.scalafromjs.esprima._

class SplitFilesTests extends AnyFunSuite with TestUtils {
  test("Simulated multiple file conversion") {
    exec check ConversionCheck(
      """
         //file:a.js
         // Some Javascript file
         function a() {}

         //file:b.js
         // Another Javascript file
         import "a.js"
         function b() {}
      """).required(
      "def a()",
      "def b()",
      "/* import \"a.js\" */"
    )

  }

  test("top-level output part is not changed by a nested token range") {
    val source = "function first(value = 1) {}\nconst second = 2 // trailing\n"
    val ast = parse(source)
    val first = ast.body.head.asInstanceOf[Node.FunctionDeclaration]
    val defaultValue = first.params.head.asInstanceOf[Node.FunctionParameterWithType].defValue
    val secondStart = source.indexOf("const second")
    // Simulate a transformed default expression whose source range belongs to
    // another composite input file.
    defaultValue.range = (secondStart, secondStart + 1)
    val second = ast.body(1)
    assert(second.trailingComments.nonEmpty)
    second.trailingComments.head.range = (0, 1)

    val firstEnd = secondStart
    val output = ScalaOut.output(
      NodeExtended(ast), source,
      ScalaOut.Config(parts = Seq(
        ScalaOut.Part(0, firstEnd, "first.js"),
        ScalaOut.Part(firstEnd, source.length, "second.js")
      ))
    )

    assert(output.head.contains("def first"))
    assert(output.head.contains("= 1)"))
    assert(!output.head.contains("// trailing"))
    assert(!output(1).contains("def first"))
    assert(output(1).contains("val second"))
    assert(output(1).contains("// trailing"))
  }


}
