package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {
  private def nodeClassName(n: AST_Node): String = {
    n.asInstanceOf[js.Dynamic].constructor.name.asInstanceOf[String]
  }

  def output(ast: AST_Node): String = {
    "<" + nodeClassName(ast) + ">"
  }

  def output(ast: AST_Block): String = {
    val statements = for (s <- ast.body) yield {
      s match {
        case s: AST_SimpleStatement =>
          output(s.body)
        case s: AST_Block =>
          "{\n" + output(s) + "}\n" // TODO: autoindent
        case s: AST_EmptyStatement =>
          ""
        case s: AST_StatementWithBody =>
          output(s.body)
        case _ =>
          output(s)
      }
    }
    statements.mkString("\n")
  }
}
