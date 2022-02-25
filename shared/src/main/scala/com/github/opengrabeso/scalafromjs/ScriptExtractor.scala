package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.esprima.Esprima

import scala.util.Try

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


  def fromHTML(name: String, html: String): Option[String] = {
    if (html.startsWith("<!DOCTYPE html")) {
      // we extract content of a <script> tag
      // we use regex for this - quick and simple, usable both on JS and JVM
      val Extract = "(?ms)<script(.*?)>(.*?)</script>".r.unanchored
      val ExtractType = """type=["'](.*?)["']""".r.unanchored
      val ExtractId = """id=["'](.*?)["']""".r.unanchored
      val matches = Extract.findAllMatchIn(html)
      if (matches.nonEmpty) Some {
        matches.flatMap { m =>
          val src = m.group(2)
          val attributes = m.group(1)
          def tryParse(src: String) = {
            val parsed = Try {
              esprima.parse(src)
            }

            parsed.failed.foreach { ex =>
              println(s"warning: script from $name not parsed, error $ex")
            }

            parsed.toOption.map { _ =>
              src
            }
          }
          attributes match {
            case ExtractType("module" | "javascript") =>
              tryParse(src)
            case ExtractType("importmap") =>
              None // maps like "three": "../build/three.module.js"
            case ExtractType(_) =>
              // explicit type, but not JS - some non-js resource (vertex shader...)
              val id = ExtractId.findFirstMatchIn(attributes).map(_.group(1)).getOrElse("htmlcontent")
              Some(wrapAsJS(id, src))
            case _ =>
              tryParse(src)
          }

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
