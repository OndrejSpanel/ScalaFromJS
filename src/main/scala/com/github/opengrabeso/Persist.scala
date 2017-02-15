package com.github.opengrabeso

import org.scalajs.dom

object Persist {
  val storage = dom.window.localStorage

  def store(name: String, value: String) = storage.setItem(name, value)

  def load(name: String): Option[String] = Option(storage.getItem(name))
}
