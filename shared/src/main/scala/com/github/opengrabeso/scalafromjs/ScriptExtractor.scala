package com.github.opengrabeso.scalafromjs

object ScriptExtractor {
  def fromHTML(html: String): Option[String] = {
    if (html.startsWith("<!DOCTYPE html")) {
      // we extract content of a <script> tag
      // we use regex for this - quick and simple, usable both on JS and JVM
      val Extract = "(?ms)<script.*?>(.*?)</script>".r.unanchored
      val matches = Extract.findAllMatchIn(html)
      if (matches.nonEmpty) Some {
        matches.map { m =>
          m.group(1)
        }.mkString("\n")
      } else None
    } else {
      None
    }

  }

  def apply(code: String): String = {
    fromHTML(code).getOrElse(code)
  }
}
