package com.github.opengrabeso

import Uglify._
import UglifyExt._
import CommandLine._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException

object ConvertProject {

  case class Item(name: String, code: String, exported: Boolean, fullName: String)

  def loadControlFile(in: String): ConvertProject = {

    val code = readFile(in)
    val project = ConvertProject(Seq(Item(in, code, true, in)))

    project.resolveImportsExports
  }
}

import ConvertProject._

case class ConvertProject(items: Seq[Item]) {
  lazy val code = items.map(_.code).mkString
  lazy val offsets = items.scanLeft(0)((offset, file) => offset + file.code.length)

  def indexOfItem(offset: Int): Int = offsets.prefixLength(_ <= offset) - 1

  def pathForOffset(offset: Int): String = {
    val index = indexOfItem(offset)
    if (items.isDefinedAt(index)) items(index).fullName
    else ""
  }

  final def resolveImportsExports: ConvertProject = {
    def readFileWithPath(path: String): (String, String) = {
      val code = readFile(path)
      // try parsing, if unable, return a comment file instead
      try {
        parse(code, defaultUglifyOptions.parse)
        code -> path
      } catch {
        case JavaScriptException(ex) if ex.isInstanceOf[JS_Parse_Error] =>
          //println(s"Parse ex: ${ex.toString} in $path")
          // TODO: embed wrapped code as a variable
          //val wrap = "// " + shortName(path) + "\n/*\n" + code + "\n*/\n"
          val wrap = "// " + shortName(path) + "\n"
          wrap -> path
      }

    }

    def readFileInProject(name: String, base: String): (String, String) = {
      // imports often miss .js extension
      val extension = ".js"
      val singlePath = resolveSibling(base, name)
      //println(s"Read file $name as $singlePath (in $base)")
      try {
        readFileWithPath(singlePath)
      } catch {
        case ex@js.JavaScriptException(ErrorCode("ENOENT" | "EISDIR")) if !singlePath.endsWith(extension) =>
          readFileWithPath(singlePath + extension)
      }
    }

    val ast = try {
      parse(code, defaultUglifyOptions.parse)
    } catch {
      case ex@JavaScriptException(err: JS_Parse_Error) =>
        println(s"Parse error $err")
        println(s"file ${err.filename} at ${err.line}:${err.col}")

        val context = code.slice(err.pos - 30, err.pos + 30)
        println(s"Context: \n$context")
        throw ex
    }

    // check export / import statements
    val importBuffer = new mutable.ArrayBuffer[(String, String)]
    val exportBuffer = new mutable.ArrayBuffer[(String, String)]
    ast.walk {
      case i: AST_Import =>
        i.start.foreach { s =>
          importBuffer append i.module_name.value -> pathForOffset(s.pos)
        }
        false
      case e: AST_Export =>
        // ignore exports in imported files
        e.start.foreach { s =>
          val index = indexOfItem(s.pos)
          //println(s"index of ${s.pos} = $index in $offsets")
          if (items.isDefinedAt(index) && items(index).exported) {
            e.module_name.foreach { name =>
              exportBuffer append name.value -> pathForOffset(s.pos)
            }
          }
        }

        false
      case _ =>
        false
    }

    // check if there are new items added into the project
    val toImport = importBuffer.filter(nb => !items.exists(_.name == nb._1))
    val toExport = exportBuffer.filter(nb => !items.exists(_.name == nb._1))
    val markAsExports = exportBuffer.filter(nb => items.exists(i => i.name == nb._1 && !i.exported)).toSet

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

      def mapFile(nb: (String, String), exported: Boolean): Item = {
        val (code, path) = readFileInProject(nb._1, nb._2)
        Item(nb._1, code, exported, path)
      }

      val old = items.map(i => i.copy(exported = i.exported || markAsExports.exists(_._1 == i.name)))
      val imports = toImport.map(nb => mapFile(nb, false))
      val exports = toExport.map(nb => mapFile(nb, true))

      //println(s"  old ${old.map(_.name).mkString(",")}")

      ConvertProject(old ++ imports ++ exports).resolveImportsExports
    }
  }

  def convert: Seq[(String, String)] = {
    val exportsImports = items.sortBy(!_.exported)
    //println(s"exportsImports ${exportsImports.map(_.copy(code = ""))}")

    if (false) { // debugging the parse - parse files one by one to pinpoint a problem location
      for (ConvertProject.Item(name, code, _, _) <- exportsImports) {
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

    for ( (outCode, ConvertProject.Item(inFile, _, _, _)) <- output zip exports) yield {
      inFile -> outCode
    }
  }
}
