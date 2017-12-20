package com.github.opengrabeso.scalafromjs

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}
import java.util.prefs.Preferences

import scala.concurrent.Future
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.MenuItem._
import scalafx.Includes._
import scalafx.scene.input._
import scalafx.scene.layout._
//import com.github.opengrabeso.scalafx.TextFieldAcceleratorFix

import scalafx.scene.image.Image

import scala.concurrent.ExecutionContext.Implicits.global

//noinspection ForwardReference
object ScalaFromJS extends JFXApp {
  def prefs: Preferences = Preferences.userRoot().node(getClass.getPackage.getName.toLowerCase)

  def fingerprint = ""

  stage = new JFXApp.PrimaryStage {
    title.value = "ScalaFromJs - Javascript to Scala conversion tool"

    width = 800
    height = 800

    //private val icon = new Image("/calculator.png")
    //icons.add(icon)

    def saveSession(): Unit = {
      val oldSize = prefs.getInt("rows", 0)
      prefs.put("version", "0")
    }


    def loadSession(): Unit = {
      val version = prefs.get("version", "")
      if (version.nonEmpty) {
      }
    }


    loadSession()

    scene = new Scene {

      var resultTimestamp = Option.empty[Long]

      val result = new TextArea {
        editable = false
      }
      val input = new TextArea {
        editable = true
        Platform.runLater(requestFocus())

        text.onChange {
          BackgroundConversion.process(text.value)
          saveSession()
        }

      }

      onCloseRequest = handle {
        BackgroundConversion.process(null)
      }

      object BackgroundConversion {
        // use actors instead?
        val inputs = new ConcurrentLinkedQueue[(String, Long)]
        val waitForInput = new Semaphore(0, true)

        def process(in: String) = {
          inputs.add(in -> System.currentTimeMillis())
          waitForInput.release()
        }

        def background(): Unit = {
          while (true) {
            waitForInput.acquire()
            var lastInput: (String, Long) = inputs.poll()
            // empty the queue
            while (waitForInput.tryAcquire() && lastInput._1 != null) {
              lastInput = inputs.poll()
            }
            if (lastInput._1 == null) return
            try {
              val resultText = Convert(lastInput._1)

              // avoid overwritting newer result
              Platform.runLater {
                if (resultTimestamp.forall(_ < lastInput._2)) {
                  result.text = resultText
                  resultTimestamp = Some(lastInput._2)
                }
              }
            } catch {
              case ex: Exception => // debug exceptions somehow?
            }
          }
        }

        val backgroundThread = new Thread(new Runnable {
          def run() = background()
        })
        backgroundThread.start()
      }

      val statusBar = new Label

      val menuBar = new MenuBar {
        useSystemMenuBar = true
        menus = Seq(
          new Menu("File") {
            items = Seq(
              new MenuItem("Clear history") {
                accelerator = new KeyCharacterCombination("N", KeyCombination.ControlDown)
                onAction = handle {}
              }
            )
          }
        )
      }

      def showStatus(): Unit = {
        statusBar.text.value = ""
      }

      val pane = new BorderPane {

        top = menuBar
        center = new HBox(input, result, statusBar)

      }

      root = pane
      showStatus()

    }
  }
}
