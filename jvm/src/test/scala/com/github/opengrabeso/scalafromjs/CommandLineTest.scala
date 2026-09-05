package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

import scala.collection.immutable.ListMap
import PathUtils._

class CommandLineTest extends AnyFunSuite with TestUtils with ProjectUtils {
  import CommandLine._
  import FileAccess._

  test("Single file conversion") {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile(rscPath("files/a.js"), temp + "aaa.scala")
      assert(out.nonEmpty)
      forEachFileWithCleanup(out) { f =>
        val outCode = readFile(f)
        exec check ResultCheck(outCode).required("def A()")
      }
    }
  }

  test("Multiple file conversion") {
    val outCode = convertProject("files/input.js")
    exec check ResultCheck(outCode)
      .required("/*", "*/", "def ", "() =", "def A", "def B", "def D")
      .forbidden("def C", "def E")
  }

  test("Inherited constructor stays in the derived output file") {
    val derivedCode =
      """
        |class Derived extends Base {
        |}
        |""".stripMargin
    val baseCode =
      """
        |class Base {
        |  constructor(value = null) {
        |  }
        |}
        |""".stripMargin

    val project = ConvertProject(
      "input.js",
      ConvertProject.ConvertConfig(),
      ListMap(
        "derived.js" -> ConvertProject.Item(derivedCode, included = true, fullName = "derived.js"),
        "base.js" -> ConvertProject.Item(baseCode, included = true, fullName = "base.js")
      )
    )

    assert(project.values.map(_.fullName) == Seq("derived.js", "base.js"))

    val convertedFiles = project.convert.files.toMap
    val derivedOutput = convertedFiles("derived.js")
    val baseOutput = convertedFiles("base.js")

    exec check ResultCheck(derivedOutput)
      .required("class Derived(", "null", "extends Base(")
      .forbidden("class Base(")
    exec check ResultCheck(baseOutput)
      .required("class Base(")
      .forbidden("class Derived(", "extends Base(")
  }

  test("Node project conversion keeps file boundaries and removes stale generated outputs") {
    withTempDir("ScalaFromJS-node-output-boundary-") { temp =>
      val outputRoot = temp + "scala/"
      val outputControl = outputRoot + "three-convert.scala"

      val firstOutputs = convertFileToFile(rscPath("nodeOutputBoundary/three-convert.js"), outputControl)
      val outputByName = firstOutputs.map(path => shortName(path) -> path).toMap

      assert(outputByName.keySet == Set(
        "three-convert.scala",
        "IESSpotLightNode.scala",
        "SpotLightNode.scala",
        "AnalyticLightNode.scala"
      ))

      val iesOutput = readFile(outputByName("IESSpotLightNode.scala"))
      val spotOutput = readFile(outputByName("SpotLightNode.scala"))
      val analyticOutput = readFile(outputByName("AnalyticLightNode.scala"))

      exec check ResultCheck(iesOutput)
        .required("class IESSpotLightNode", "extends SpotLightNode")
        .forbidden("class SpotLightNode", "class AnalyticLightNode")
      exec check ResultCheck(spotOutput)
        .required("class SpotLightNode", "extends AnalyticLightNode")
        .forbidden("class IESSpotLightNode", "class AnalyticLightNode")
      exec check ResultCheck(analyticOutput)
        .required("class AnalyticLightNode")
        .forbidden("class IESSpotLightNode", "class SpotLightNode")

      val staleOutput = outputRoot + "src/nodes/lighting/RemovedNode.scala"
      mkAllDirs(staleOutput)
      writeFile(staleOutput, "/*\nScalaFromJS: 0.8.0\nRemovedNode.js\n*/\nclass RemovedNode\n")
      val handMaintained = outputRoot + "src/nodes/lighting/HandMaintained.scala"
      writeFile(handMaintained, "class HandMaintained\n")

      val secondOutputs = convertFileToFile(rscPath("nodeOutputBoundary/three-convert.js"), outputControl)

      assert(secondOutputs.toSet == firstOutputs.toSet)
      assert(!Files.exists(Paths.get(staleOutput)))
      assert(Files.exists(Paths.get(handMaintained)))
    }
  }

  test("Multiple file conversion with non-js files") {
    val outCode = convertProject("nonJSFiles/input.js")
    exec check ResultCheck(outCode)
      .required(
        "This is a plain text file, to be packed verbatim, as data.",
        "var value =",
        "object plain extends Resource {"
      ).forbidden(
        "isResource"
      )
  }

  test("Multiple file conversion with rules") {
    val outCode = convertProject("folderRules/input.js")
    exec check ResultCheck(outCode)
      .required(
        "package my.name"
      ).forbidden(
        "package a.js",
        "package src",
        "package test"
      )
  }

  test("Multiple file conversion with wrong rules") {
    intercept[UnsupportedOperationException] {
      convertProject("folderRules/ruleError.js")
    }
  }

  test("Multiple file conversion with file wrapping") {
    val outCode = convertProject("folderRules/wrapFile.js")
    exec check ResultCheck(outCode)
      .required(
        "package my.name",
        "class SomeTest extends Tests",
        "object dd extends Data",
        "val value =",
        "import something."
      ).forbidden(
        "package a.js",
        "package src",
        "package test"
      )
  }
}
