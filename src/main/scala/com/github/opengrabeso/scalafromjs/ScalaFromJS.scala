package com.github.opengrabeso.scalafromjs

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}
import java.util.prefs.Preferences

import scala.concurrent.Future
import scala.util.Try
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

  stage = new JFXApp.PrimaryStage {
    title.value = "ScalaFromJs - Javascript to Scala conversion tool"

    width = 800
    height = 800

    //private val icon = new Image("/calculator.png")
    //icons.add(icon)


    scene = new Scene {

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

      def saveSession(): Unit = {
        prefs.put("version", "0")
        prefs.put("input", input.text.value)
      }


      def loadSession(): Unit = {
        val version = prefs.get("version", "")
        if (version.nonEmpty) {
          val loaded = prefs.get("input", "")
          input.text.value = loaded
        }
      }


      loadSession()

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

          def now() = System.currentTimeMillis()

          while (true) {
            waitForInput.acquire()
            var lastInput: (String, Long) = inputs.poll()
            // empty the queue
            while (waitForInput.tryAcquire() && lastInput._1 != null) {
              lastInput = inputs.poll()
            }
            if (lastInput._1 == null) return
            val start = now()
            val resultText = Try (Convert(lastInput._1))
            val duration = now() - start

            // avoid overwriting newer result
            Platform.runLater {
              resultText.map { text =>
                result.text = text
                statusBar.text.value = s"Conversion duration $duration ms"
              }.failed.map { ex =>
                statusBar.text.value = s"Conversion duration $duration ms, error ${ex.toString}"
              }

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
        center = new HBox(input, result)
        bottom = statusBar

      }

      root = pane
      showStatus()

    }
  }
}
