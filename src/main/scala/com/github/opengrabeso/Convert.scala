package com.github.opengrabeso

import Uglify._
import UglifyExt._

object Convert {
  def apply(code: String, header: Boolean = true): String = {
    val ast = parse(code, defaultUglifyOptions.parse)
    val astOptimized = Transform(ast)
    val prefix = if (header) s"/* ${Main.fingerprint()}*/\n\n" else ""
    prefix + ScalaOut.output(astOptimized, code)
  }


}
