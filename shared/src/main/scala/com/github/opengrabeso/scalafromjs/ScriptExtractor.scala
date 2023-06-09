package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.esprima.Esprima

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

object ScriptExtractor {
  def wrapAsJS(filename: String, value: String): String = {
    // embed wrapped code as a variable using ES6 template string
    // use isResource so that Scala output can check it and handle it as a special case
    val jsName = filename.filter(_.isLetterOrDigit)
    s"""
       |const $jsName = {
       |  value: `$value`,
       |  isResource: true
       |}
       |""".stripMargin
  }

  object ImportMapParser extends JavaTokenParsers {
    def map: Parser[Map[String, String]] = "{" ~> "\"imports\"" ~> ":" ~> "{" ~> repsep(mapping, ",") <~ "}" <~ "}" ^^ (Map() ++ _)
    def mapping: Parser[(String, String)] = stringLiteral ~ ":" ~ stringLiteral ^^ {
      case key ~ ":" ~ value => (key.replaceAll("\"", ""), value.replaceAll("\"", ""))
    }

    def parse(str: String): Map[String, String] = parseAll(map, str) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  def parseImportMap(src: String): Map[String, String] = {
    ImportMapParser.parse(src)
  }

  def fromHTML(name: String, html: String): Option[String] = {
    if (html.startsWith("<!DOCTYPE html")) {
      // we extract content of a <script> tag
      // we use regex for this - quick and simple, usable both on JS and JVM
      val Extract = "(?ms)<script(.*?)>(.*?)</script>".r.unanchored
      val ExtractType = """type=["'](.*?)["']""".r.unanchored
      val ExtractId = """id=["'](.*?)["']""".r.unanchored
      def tryParse(src: String) = {
        val parsed = Try(esprima.parse(src))

        parsed.failed.foreach { ex =>
          println(s"warning: script from $name not parsed, error $ex")
        }
        Option.when(parsed.isSuccess)(src)
      }
      val matches = Extract.findAllMatchIn(html)
      if (matches.nonEmpty) Some {
        val groups = matches.map(m => m.group(1) -> m.group(2)).toSeq
        val importMap = groups.collect {
          case (ExtractType("importmap"), src) =>
            parseImportMap(src)
        }.flatten.toMap
        def applyImportMaps(code: String): String = {
          val lines = code.split("\n")
          val sortedImportMap = importMap.toSeq.sortBy(-_._1.length) // Sort by key length in descending order, to make sure three/addons matches before three
          val importLines = lines.map { line =>
            if (line.trim.startsWith("import")) {
              sortedImportMap.foldLeft(line)((currentLine, mapping) =>
                currentLine.replaceAll("\\b" + Regex.quote(mapping._1) + "\\b", mapping._2)
              )
            } else {
              line
            }
          }
          importLines.mkString("\n")

        }
        groups.flatMap {
          case (ExtractType("module" | "javascript"), src) =>
            tryParse(src).map(applyImportMaps)
          case (attributes@ExtractType(t), src) =>
            Option.when (t != "importmap") {
              // explicit type, but not JS - some non-js resource (vertex shader...)
              val id = ExtractId.findFirstMatchIn(attributes).map(_.group(1)).getOrElse("htmlcontent")
              wrapAsJS(id, src)
            }
          case (_, src) =>
            tryParse(src).map(applyImportMaps)
        }.mkString("\n")
      } else None
    } else {
      None
    }

  }

  def apply(code: String): String = {
    fromHTML("HTML", code).getOrElse(code)
  }
}
