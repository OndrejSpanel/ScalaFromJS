package com.github.opengrabeso.scalafromjs

import org.scalajs.dom

object GlobalConfig {
  lazy val fingerprint = {
    val name = dom.window.location.host + dom.window.location.pathname
    val version = "Web"
    name + ": " + version
  }

}
