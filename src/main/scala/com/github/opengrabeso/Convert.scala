package com.github.opengrabeso

import Uglify._
import UglifyExt._

object Convert {
  def apply(code: String, header: Boolean = true): String = {
    val prefix = if (header) s"/* ScalaFromJS build ${Main.version()}*/\n\n" else ""
    val ast = parse(code, defaultUglifyOptions.parse)
    ast.figure_out_scope()
    val astOptimized = Transform(ast)
    prefix + ScalaOut.output(astOptimized, code)
  }


}
