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

  trait DelayedAction[T] {
    def action(pars: T): Unit

    // when multiple conversions are pending, execute only the one triggered last
    var lastTriggered = Option.empty[Double]

    def trigger(timestamp: Double, delayMs: Int, pars: T) = {
      if (lastTriggered.isEmpty || lastTriggered.get < timestamp) {
        lastTriggered = Some(timestamp)
      }

      setTimeout(delayMs) {
        callback(timestamp, pars)
      }
    }

    def callback(timestamp: Double, pars: T): Unit = {
      if (lastTriggered.contains(timestamp)) {
        action(pars)
      }
    }
  }

  object DelayedConversion extends DelayedAction[Unit] {
    def action(pars: Unit) = doConversion()
  }

  object DelayedOutput extends DelayedAction[String] {
    def action(result: String) = out.asInstanceOf[js.Dynamic].value = result
  }

  def convert(code: String): String = {
    val ast = parse(code, defaultUglifyOptions.parse)
    ast.figure_out_scope()
    val astOptimized = Transform(ast)
    ScalaOut.output(astOptimized, code)
  }

  def doConversion(persist: Boolean = true) = {
    val code = in.asInstanceOf[js.Dynamic].value.asInstanceOf[String]

    Persist.store("source", code)

    Try(convert(code)).fold(
      { err =>
        // if result is an error, wait before displaying it
        // this prevents error flashing while typing
        DelayedOutput.trigger(System.currentTimeMillis, 2000, err.getLocalizedMessage)
      }, { scalaCode =>
        DelayedOutput.trigger(System.currentTimeMillis, 0, scalaCode)
      }
    )
  }

  private def onInput(e: Event) = {
    val code = in.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
    // longer delay on a long code
    val delayMs = 10 max code.length / 100 min 1000
    DelayedConversion.trigger(e.timeStamp, delayMs, ())
  }

  private def onPaste(e: Event) = {
    // no delay needed after paste
    DelayedConversion.trigger(e.timeStamp, 10, ())
  }


  private def windowLoaded(e: Event) = {
    in.addEventListener("input", onInput)
    in.addEventListener("paste", onPaste)

    val previous = Persist.load("source")
    for (p <- previous) {
      in.asInstanceOf[js.Dynamic].value = p
      doConversion(false)
    }

  }

  @JSExport
  def main(): Unit = {

    dom.window.addEventListener("load", windowLoaded)
  }
}
