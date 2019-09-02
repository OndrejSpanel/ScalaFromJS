package com.github.opengrabeso.scalafromjs

import javax.swing.KeyStroke

import scala.swing.Action

class ActionWithCode(name: String, code: => Unit) extends Action(name) {
  def apply() = code

  def this(name: String, code: => Unit, keyStroke: Option[KeyStroke]) = {
    this(name, code)
    accelerator = keyStroke
  }
  def this(name: String, code: => Unit, keyCode: Int, modifiers: Int = 0) = {
    this(name, code, Some(KeyStroke.getKeyStroke(keyCode, modifiers)))
  }

  def selected = {
    val s = peer.getValue(javax.swing.Action.SELECTED_KEY)
    s.asInstanceOf[Boolean]
  }
  def selected_=(sel: Boolean) = peer.putValue(javax.swing.Action.SELECTED_KEY, sel)
}
