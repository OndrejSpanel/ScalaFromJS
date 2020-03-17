package com.github.opengrabeso.scalafromjs

import org.scalajs.dom

object GlobalConfig {
  lazy val fingerprint = {
    val p = Convert.getClass.getName //.reverse.dropWhile(_ == '$').reverse
    val name = p.split('.').dropRight(1).mkString(".")
    val version = "Web"
    val timestamp = dom.document.lastModified
    name + ": " + version + " " + timestamp
  }

}
