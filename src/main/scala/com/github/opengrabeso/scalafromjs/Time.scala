package com.github.opengrabeso.scalafromjs

/**
  * Created by Ondra on 4.4.2017.
  */
object Time {
  def apply[T](message: => String)(code: => T): T = {
    val log = false
    val now = System.currentTimeMillis()
    val ret = code
    if (log) println(s"$message ${System.currentTimeMillis() - now} ms")
    ret
  }

  def disabled[T](message: => String)(code: => T): T = {
    code
  }
}
