package com.github.opengrabeso.scalafromjs

import org.scalajs.dom

import scala.util.Try

object Persist {
  val enableStorage = true

  val storage = if (enableStorage) Try { Option(dom.window.localStorage) }.toOption.flatten else None

  def store(name: String, value: String): Unit = storage.foreach(_.setItem(name, value))

  def load(name: String): Option[String] = storage.map(_.getItem(name))
}
