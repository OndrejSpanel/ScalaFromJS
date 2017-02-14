package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.Event

object Main extends js.JSApp {

  def onInput(e: Event) = {
    println("Input changed")
  }


  def windowLoaded(e: Event) = {
    //dom.window.alert("Hi from Scala-js-dom")
    dom.document.getElementById("in").addEventListener("input", onInput)
  }

  @JSExport
  def main(): Unit = {

    dom.window.addEventListener("load", windowLoaded)
  }
}
