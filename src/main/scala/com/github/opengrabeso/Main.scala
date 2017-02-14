package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.timers._
import org.scalajs.dom
import org.scalajs.dom.Event

import scala.util.Try

object Main extends js.JSApp {

  private lazy val in = dom.document.getElementById("in")
  private lazy val out = dom.document.getElementById("out")

  object DelayedConversion {
    // when multiple conversions are pending, execute only the one triggered last
    var lastTriggered = Option.empty[Double]

    def trigger(timestamp: Double, delayMs: Int) = {
      if (lastTriggered.isEmpty || lastTriggered.get < timestamp) {
        lastTriggered = Some(timestamp)
      }

      setTimeout(delayMs) {
        callback(timestamp)
      }
    }

    def callback(timestamp: Double): Unit = {
      if (lastTriggered.contains(timestamp)) {
        doConversion()
      }
    }
  }

  def doConversion() {
    val code = in.asInstanceOf[js.Dynamic].value.asInstanceOf[String]

    Try {
      val ast = parse(code, defaultUglifyOptions.parse)
      val astOptimized = ast.optimize(defaultOptimizeOptions)
      ScalaOut.output(astOptimized, code)
    }.foreach { scalaCode =>
      out.asInstanceOf[js.Dynamic].value = scalaCode
    }
  }

  private def onInput(e: Event) = {
    val code = in.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
    // longer delay on a long code
    val delayMs = 10 max code.length / 100 min 1000
    println(delayMs)
    DelayedConversion.trigger(e.timeStamp, delayMs)
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
