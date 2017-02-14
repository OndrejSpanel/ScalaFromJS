package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.Event

object Main extends js.JSApp {

  private lazy val in = dom.document.getElementById("in")
  private lazy val out = dom.document.getElementById("out")

  private def onInput(e: Event) = {
    val code = in.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
    val ast = parse(code, defaultUglifyOptions.parse)

    val astOptimized = ast.optimize(defaultOptimizeOptions)
    val scalaCode = ScalaOut.output(astOptimized, code)

    out.asInstanceOf[js.Dynamic].value = scalaCode
  }


  def windowLoaded(e: Event) = {
    //dom.window.alert("Hi from Scala-js-dom")
    in.addEventListener("input", onInput)
  }

  @JSExport
  def main(): Unit = {

    dom.window.addEventListener("load", windowLoaded)
  }
}
