package com.github.opengrabeso.scalafromjs

object GlobalConfig {
  lazy val fingerprint = {
    // https://stackoverflow.com/a/13546822/16673
    val p = Convert.getClass.getPackage
    val name = Option(p.getImplementationTitle).getOrElse("ScalaFromJS")
    val version = Option(p.getImplementationVersion).getOrElse("Dev")
    // Uglify Scala.js version was named like this: ScalaFromJS: 2017-11-22 17:48:10.567
    name + ": " + version
  }

}
