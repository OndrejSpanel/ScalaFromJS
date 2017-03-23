package com.github.opengrabeso

import org.scalajs.dom

import scala.util.Try

object Persist {
  val storage = Try { Option(dom.window.localStorage) }.toOption.flatten

  def store(name: String, value: String) = storage.foreach(_.setItem(name, value))

  def load(name: String): Option[String] = storage.map(_.getItem(name))
}
