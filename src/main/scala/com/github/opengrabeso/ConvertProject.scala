package com.github.opengrabeso

import Uglify._
import UglifyExt._
import CommandLine._
import com.github.opengrabeso.Transform.AST_Extended

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException

object ConvertProject {

  case class Item(code: String, exported: Boolean, fullName: String) {
    override def toString = s"($fullName:$exported)"
  }

  def loadControlFile(in: String): ConvertProject = {
    Time("loadControlFile") {
      val code = readFile(in)
      val project = ConvertProject(ListMap(in -> Item(code, true, in)))

      project.resolveImportsExports
    }
  }
}

import ConvertProject._

case class ConvertProject(items: Map[String, Item]) {
  lazy val values = items.values.toIndexedSeq
  lazy val code = values.map(_.code).mkString
  lazy val offsets = values.scanLeft(0)((offset, file) => offset + file.code.length)

  def indexOfItem(offset: Int): Int = offsets.prefixLength(_ <= offset) - 1

  def pathForOffset(offset: Int): String = {
    val index = indexOfItem(offset)
    if (values.isDefinedAt(index)) values(index).fullName
    else ""
  }

  final def resolveImportsExports: ConvertProject = {
    def readFileAsJs(path: String): (String, String) = {
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


    /**@return (content, path) */
    def readJsFile(name: String): (String, String) = {
      // first try if it is already loaded

      // imports often miss .js extension
      val extension = ".js"
      items.get(name).orElse(items.get(name + extension)).fold {
        //println(s"Read file $name as $singlePath (in $base)")
        try {
          readFileAsJs(name)
        } catch {
          case ex@js.JavaScriptException(ErrorCode("ENOENT" | "EISDIR")) if !name.endsWith(extension) =>
            readFileAsJs(name + extension)
        }
      } { item =>
        item.code -> item.fullName
      }
    }

    val ast = try {
      //println("** Parse\n" + items.mkString("\n"))
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
          importBuffer append readJsFile(resolveSibling(pathForOffset(s.pos), i.module_name.value))
        }
        false
      case e: AST_Export =>
        // ignore exports in imported files
        e.start.foreach { s =>
          val index = indexOfItem(s.pos)
          //println(s"index of ${s.pos} = $index in $offsets")
          if (values.isDefinedAt(index) && values(index).exported) {
            e.module_name.foreach { name =>
              exportBuffer append readJsFile(resolveSibling(pathForOffset(s.pos), name.value))
            }
          }
        }

        false
      case _ =>
        false
    }

    // check if there are new items added into the project
    val toImport = importBuffer.filter(name => !items.contains(name._2))
    val toExport = exportBuffer.filter(name => !items.contains(name._2))
    val markAsExports = exportBuffer.filter(name => items.get(name._2).exists(!_.exported)).toSet

    if (toImport.isEmpty && toExport.isEmpty && markAsExports.isEmpty) {
      //println(s"Do not add to ${items.map(_.name).mkString(",")}")
      this
    } else {

      if (false) {
        println(s"Add to ${items.mapValues(_.fullName).mkString("(", "\n", ")")}")
        println(s"  imports ${toImport.map(_._2).mkString("(", "\n", ")")}")
        println(s"  exports ${toExport.map(_._2).mkString("(", "\n", ")")}")
        println(s"  markAsExports ${markAsExports.mkString("(", "\n", ")")}")
      }

      def mapFile(cp: (String, String), exported: Boolean): (String, Item) = {
        val (code, path) = cp
        path -> Item(code, exported, path)
      }

      val old = items.mapValues(i => i.copy(exported = i.exported || markAsExports.exists(_._2 == i.fullName)))
      val imports = toImport.map(nb => mapFile(nb, false))
      val exports = toExport.map(nb => mapFile(nb, true))

      assert((old.keySet intersect imports.map(_._1).toSet).isEmpty)
      assert((old.keySet intersect exports.map(_._1).toSet).isEmpty)
      //println(s"  old ${old.map(_.name).mkString(",")}")

      ConvertProject(old ++ imports ++ exports).resolveImportsExports
    }
  }

  def convert: Seq[(String, String)] = {
    val exportsImports = values.sortBy(!_.exported)
    //println(s"exportsImports ${exportsImports.map(_.copy(code = ""))}")

    if (false) { // debugging the parse - parse files one by one to pinpoint a problem location
      for (ConvertProject.Item(code, _, name) <- exportsImports) {
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

    val ast = Time(s"Parse ${compositeFile.lines.length} lines") {
      parse(compositeFile, defaultUglifyOptions.parse)
    }

    val astOptimized = if (false) Transform(ast) else AST_Extended(ast, SymbolTypes())
    val outConfig = ScalaOut.Config().withParts(fileOffsets drop 1)
    //println(s"$outConfig")
    val output = ScalaOut.output(astOptimized, compositeFile, outConfig)

    for ( (outCode, ConvertProject.Item(_, _, inFile)) <- output zip exports) yield {
      inFile -> outCode
    }
  }
}
