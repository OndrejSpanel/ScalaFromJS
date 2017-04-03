package com.github.opengrabeso

import Uglify._
import UglifyExt._

import CommandLine._

import scala.collection.mutable

object ConvertProject {

  case class Item(name: String, code: String, exported: Boolean)

  def loadControlFile(in: String): ConvertProject = {

    val code = readFile(in)
    val project = ConvertProject(Seq(Item(shortName(in), code, true)))

    project.resolveImportsExports(in)
  }
}

import ConvertProject._

case class ConvertProject(items: Seq[Item]) {
  def resolveImportsExports(in: String): ConvertProject = {
    def readFileInProject(name: String) = {
      val singlePath = resolveSibling(in, name)
      readFile(singlePath.toString)
    }

    val code = items.map(_.code).mkString

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

    // check if there are new items added into the project
    val toImport = importBuffer.filter(name => !items.exists(_.name == name))
    val toExport = exportBuffer.filter(name => !items.exists(_.name == name))
    val markAsExports = exportBuffer.filter(name => items.exists(i => i.name == name && !i.exported)).toSet

    if (toImport.isEmpty && toExport.isEmpty && markAsExports.isEmpty) {
      println(s"Do not add to ${items.map(_.name).mkString(",")}")
      this
    } else {

      println(s"Add to ${items.map(_.name).mkString(",")}")
      println(s"  imports ${toImport.mkString(",")}")
      println(s"  exports ${toExport.mkString(",")}")
      println(s"  markAsExports ${markAsExports.mkString(",")}")

      val old = items.map(i => i.copy(exported = i.exported || markAsExports.contains(i.name)))
      val imports = toImport.map(name => Item(name, readFileInProject(name), false))
      val exports = toExport.map(name => Item(name, readFileInProject(name), true))

      println(s"  old ${old.map(_.name).mkString(",")}")

      ConvertProject(old ++ imports ++ exports).resolveImportsExports(in)
    }

  }
}
