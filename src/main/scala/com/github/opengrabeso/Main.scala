package com.github.opengrabeso

import com.github.opengrabeso.Esprima._
import com.github.opengrabeso.JsonToString._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

object Main extends js.JSApp {
  @JSExport
  def main(): Unit = {
    println("Testing ...")
    val code = "answer = 42"
    val tokens = tokenize(code)
    println(s"Tokens: ${tokens.json}")
    val parsed = parse(code)
    println(s"Parsed: ${parsed.json}")
    println("Done.")
  }
}
