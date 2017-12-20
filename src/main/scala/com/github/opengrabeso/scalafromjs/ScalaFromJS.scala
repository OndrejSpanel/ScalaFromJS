package com.github.opengrabeso.scalafromjs

import java.util.prefs.Preferences

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

//noinspection ForwardReference
object ScalaFromJS extends JFXApp {
  def prefs: Preferences = Preferences.userRoot().node(getClass.getPackage.getName.toLowerCase)

  def fingerprint = ""

  stage = new JFXApp.PrimaryStage {
    title.value = "ScalaFromJs - Javascript to Scala conversion tool"


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
      def computeResult(preview: Boolean): Unit = {
        val resultText = Convert(input.text.value)
        result.text = resultText
      }

      val result = new TextArea {
        editable = false
      }
      val input = new TextArea {
        editable = true
        Platform.runLater(requestFocus())

        text.onChange {
          computeResult(true)
          saveSession()
        }

      }
      val statusBar = new Label

      def changeSettings(change: => Unit): Unit = {
        change
        showStatus()
        computeResult(true)

      }

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
