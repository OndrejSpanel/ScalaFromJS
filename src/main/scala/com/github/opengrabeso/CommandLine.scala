package com.github.opengrabeso

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._

object CommandLine {

  val fs = g.require("fs")
  val process = g.require("process")

  // TODO: facade instead of Dynamic

  def readFile(name: String): String = {
    fs.readFileSync(name).toString
  }

  def writeFile(name: String, content: String): Unit = {
    fs.writeFileSync(name, content)
  }

  lazy val argv: Seq[String] = {
    process.argv.asInstanceOf[js.Array[String]]
  }

  def convertFileToFile(in: String, out: String): Unit = {
    val source = readFile(in)
    val output = Convert(source)
    writeFile(out, output)
  }

  def apply() = {
    println("Node.js - Command line")
    println(s"  args ${argv.mkString(",")}")

    convertFileToFile("temp/input.js", "temp/output.scala")

  }
}
