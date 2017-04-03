package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.collection.mutable

object ConvertProject {
  import CommandLine.readFile
  import CommandLine.resolveSibling

  case class Item(name: String, code: String, exported: Boolean)

  def loadControlFile(in: String): ConvertProject = {

    def readFileInProject(name: String) = {
      val singlePath = resolveSibling(in, name)
      readFile(singlePath.toString)
    }

    val code = readFile(in)

    val ast = parse(code, defaultUglifyOptions.parse)

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
      val imports = importBuffer.map(name => Item(name, readFileInProject(name), false))
      val exports = importBuffer.map(name => Item(name, readFileInProject(name), true))

      ConvertProject(imports ++ exports)
    } else {
      ConvertProject(Seq(Item(in, code, true)))
    }
  }
}

import ConvertProject._

case class ConvertProject(items: Seq[Item])
