package com.github.opengrabeso

import Uglify._
import UglifyExt._

object Convert {
  def apply(code: String): String = {
    val ast = parse(code, defaultUglifyOptions.parse)
    ast.figure_out_scope()
    val astOptimized = Transform(ast)
    ScalaOut.output(astOptimized, code)
  }


}
