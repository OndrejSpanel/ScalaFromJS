package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import scala.collection.mutable
import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object CommandLine {

  val fs = g.require("fs")
  val process = g.require("process")

  // TODO: facade instead of Dynamic

  def readFile(name: String): String = {
    fs.readFileSync(name).toString
  }

  def writeFile(name: String, content: String): Unit = {
    fs.writeFileSync(name, content)
  }

  lazy val argv: Seq[String] = {
    process.argv.asInstanceOf[js.Array[String]]
  }

  case class ConvertProject(imports: Seq[String], exports: Seq[String])

  def loadControlFile(ast: AST_Toplevel): Try[ConvertProject] = {
    // check export / import statements
    val importBuffer = new mutable.ArrayBuffer[String]
    val exportBuffer = new mutable.ArrayBuffer[String]
    ast.walk {
      case i: AST_Import =>
        importBuffer append i.module_name.value
        false
      case e: AST_Export =>
        e.module_name.foreach { name =>
          exportBuffer append name.value
        }
        false
      case _ =>
        false

    }
    if (exportBuffer.nonEmpty) {
      Success(ConvertProject(importBuffer, exportBuffer))
    } else {
      Failure(new NoSuchElementException)
    }
  }

  def resolveSibling(path: String, short: String): String = {
    val dir = path.lastIndexOf('/')
    if (dir <= 0) short
    else path.take(dir + 1) + short
  }

  def convertFileToFile(in: String, out: String): Unit = {
    val code = readFile(in)

    val ast = parse(code, defaultUglifyOptions.parse)

    val controlFile = loadControlFile(ast)
    val prefix = s"/* ${Main.fingerprint()}*/\n\n"

    controlFile.toOption.fold{
      val astOptimized = Transform(ast)
      val output = prefix + ScalaOut.output(astOptimized, code).mkString
      writeFile(out, output)
    }{ project =>
      val inputFileNames = project.exports ++ project.imports

      def loadFiles(names: Seq[String]) = names.map { filename =>
        val singlePath = resolveSibling(in, filename)
        readFile(singlePath.toString)
      }

      val exportsFiles = loadFiles(project.exports)
      val importsFiles = loadFiles(project.imports)


      val fileOffsets = exportsFiles.scanLeft(0){ (offset, file) =>
        offset + file.length
      }
      println(fileOffsets zip (project.exports :+ ""))

      val compositeFile = (exportsFiles ++ importsFiles).mkString

      val ast = parse(compositeFile, defaultUglifyOptions.parse)

      val astOptimized = Transform(ast)
      val output = prefix + ScalaOut.output(astOptimized, code).mkString("\n/*|*/\n")
      // TODO: process output
      writeFile(out, output)
    }
  }

  def apply() = {
    println(s"  args ${argv.mkString(",")}")

    convertFileToFile("temp/input.js", "temp/output.scala")

  }
}
