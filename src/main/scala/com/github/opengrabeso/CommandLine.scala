package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import scala.collection.mutable
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
    if (dir < 0) short
    else path.take(dir + 1) + short
  }

  def shortName(path: String): String = {
    val dir = path.lastIndexOf('/')
    if (dir < 0) path
    path.drop(dir + 1)
  }

  def extension(path: String): String = {
    val short = shortName(path)
    val ext = short.lastIndexOf('.')
    if (ext<0) ""
    else short.drop(ext + 1)
  }
  def changeExtension(path: String, pathWithExtension: String): String = {
    val ext = extension(path)
    path.dropRight(ext.length) + extension(pathWithExtension)
  }

  def convertFileToFile(in: String, out: String): Unit = {
    val code = readFile(in)

    val ast = parse(code, defaultUglifyOptions.parse)

    val controlFile = loadControlFile(ast)
    controlFile.toOption.fold{
      val astOptimized = Transform(ast)
      val output = s"/* ${Main.fingerprint()}*/\n\n" + ScalaOut.output(astOptimized, code).mkString
      writeFile(out, output)
    }{ project =>
      def loadFiles(names: Seq[String]) = names.map { filename =>
        val singlePath = resolveSibling(in, filename)
        readFile(singlePath.toString)
      }


      val exportsFiles = loadFiles(project.exports)
      val importsFiles = loadFiles(project.imports)


      val fileOffsets = exportsFiles.scanLeft(0)((offset, file) => offset + file.length)
      println(fileOffsets.drop(1) zip project.exports)

      val compositeFile = (exportsFiles ++ importsFiles).mkString

      val ast = parse(compositeFile, defaultUglifyOptions.parse)

      val astOptimized = Transform(ast)
      val outConfig = ScalaOut.Config().withParts(fileOffsets drop 1)
      val output = ScalaOut.output(astOptimized, code, outConfig)

      for ( (outCode, inFile) <- output zip project.exports) {

        val outFileBase = resolveSibling(out, inFile)

        val outFileCombined = changeExtension(outFileBase, out)

        val extendedPrefix = s"/*\n${Main.fingerprint()}\n${shortName(inFile)}\n*/\n\n"
        println(s"Write $outFileCombined from $inFile (out: $out)")
        writeFile(outFileCombined, extendedPrefix + outCode)
      }
    }
  }

  def apply() = {
    println(s"  args ${argv.mkString(",")}")

    convertFileToFile("temp/input.js", "temp/output.scala")

  }
}
