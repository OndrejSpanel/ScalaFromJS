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
  lazy val code = items.map(_.code).mkString
  lazy val offsets = items.scanLeft(0)((offset, file) => offset + file.code.length)

  def indexOfItem(offset: Int): Int = offsets.prefixLength(_ <= offset) - 1

  final def resolveImportsExports(in: String): ConvertProject = {
    def readFileInProject(name: String) = {
      val singlePath = resolveSibling(in, name)
      readFile(singlePath.toString)
    }

    val ast = parse(code, defaultUglifyOptions.parse)

    // check export / import statements
    val importBuffer = new mutable.ArrayBuffer[String]
    val exportBuffer = new mutable.ArrayBuffer[String]
    ast.walk {
      case i: AST_Import =>
        importBuffer append i.module_name.value
        false
      case e: AST_Export =>
        // ignore exports in imported files
        e.start.foreach { s =>
          val index = indexOfItem(s.pos)
          //println(s"index of ${s.pos} = $index in $offsets")
          if (items.isDefinedAt(index) && items(index).exported) {
            e.module_name.foreach { name =>
              exportBuffer append name.value
            }
          }
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
      //println(s"Do not add to ${items.map(_.name).mkString(",")}")
      this
    } else {

      /*
      println(s"Add to ${items.map(_.name).mkString(",")}")
      println(s"  imports ${toImport.mkString(",")}")
      println(s"  exports ${toExport.mkString(",")}")
      println(s"  markAsExports ${markAsExports.mkString(",")}")
      */

      val old = items.map(i => i.copy(exported = i.exported || markAsExports.contains(i.name)))
      val imports = toImport.map(name => Item(name, readFileInProject(name), false))
      val exports = toExport.map(name => Item(name, readFileInProject(name), true))

      //println(s"  old ${old.map(_.name).mkString(",")}")

      ConvertProject(old ++ imports ++ exports).resolveImportsExports(in)
    }

  }


  def convert: Seq[(String, String)] = {
    val exportsImports = items.sortBy(!_.exported)
    //println(s"exportsImports ${exportsImports.map(_.copy(code = ""))}")

    if (false) { // debugging the parse - parse files one by one to pinpoint a problem location
      for (ConvertProject.Item(name, code, _) <- exportsImports) {
        try {
          println(s"Parse $name")
          parse(code, defaultUglifyOptions.parse)
        } catch {
          case util.control.NonFatal(ex) =>
            ex.printStackTrace()
        }
      }
    }

    val exports = exportsImports.takeWhile(_.exported)

    val fileOffsets = exports.scanLeft(0)((offset, file) => offset + file.code.length)
    //println(fileOffsets.drop(1) zip project.exports)

    val compositeFile = exportsImports.map(_.code).mkString

    //println(s"Parse all {{$compositeFile}}")
    val ast = parse(compositeFile, defaultUglifyOptions.parse)
    //println("Parse done")

    val astOptimized = Transform(ast)
    val outConfig = ScalaOut.Config().withParts(fileOffsets drop 1)
    //println(s"$outConfig")
    val output = ScalaOut.output(astOptimized, compositeFile, outConfig)

    for ( (outCode, ConvertProject.Item(inFile, _, _)) <- output zip exports) yield {
      inFile -> outCode
    }
  }
}
