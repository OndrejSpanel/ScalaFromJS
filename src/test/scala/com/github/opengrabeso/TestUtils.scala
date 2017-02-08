package com.github.opengrabeso

import scala.concurrent.{Promise, Future}
import scala.scalajs.js
import js.Dynamic.{global => g}

object TestUtils {

  lazy val fs = g.require("fs")

  def textResource(path: String): Future[String] = {
    val p = Promise[String]()
    fs.readFile("src/test/resources/" + path, "utf-8", {
      (err: js.Dynamic, data: js.Dynamic) =>
        if (js.isUndefined(err) || err == null) {
          p.success(data.asInstanceOf[String])
        }
        else {
          p.failure(new Exception(err.toString))
        }
        ()
    }: js.Function2[js.Dynamic, js.Dynamic, Unit])
    p.future
  }
}
