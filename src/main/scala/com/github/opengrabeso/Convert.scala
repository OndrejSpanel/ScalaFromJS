package com.github.opengrabeso

import Uglify._
import UglifyExt._

object Convert {
  def prefix(header: Boolean) = if (header) s"/* ${ScalaFromJS.fingerprint()}*/\n\n" else ""

  def apply(code: String, header: Boolean = true): String = {
    val ast = parse(code, defaultUglifyOptions.parse)


    val ext = Transform.AST_Extended(ast).loadConfig

    val astOptimized = Transform(ext)
    prefix(header) + ScalaOut.output(astOptimized, code).mkString
  }

  def project(in: String, header: Boolean = true): String = {
    val project = ConvertProject.loadControlFile(in)
    val converted = project.convert.files.map(_._2).mkString
    prefix(header) + converted
  }


}
