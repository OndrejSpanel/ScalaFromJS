package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConvertProject {
  def loadControlFile(ast: AST_Toplevel): Try[ConvertProject] = {
    // check export / import statements
    val importBuffer = new mutable.ArrayBuffer[String]
    val exportBuffer = new mutable.ArrayBuffer[String]
    ast.walk {
      case i: AST_Import =>
        importBuffer append i.module_name.value
        false
      case e: AST_Export =>
        e.module_name.foreach { name =>
          exportBuffer append name.value
        }
        false
      case _ =>
        false

    }
    if (exportBuffer.nonEmpty) {
      Success(ConvertProject(importBuffer, exportBuffer))
    } else {
      Failure(new NoSuchElementException)
    }
  }
}

case class ConvertProject(imports: Seq[String], exports: Seq[String])

