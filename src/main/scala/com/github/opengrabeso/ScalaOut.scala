package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

object ScalaOut {
  def output(ast: AST_Node): String = {
    "<node>"
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
      }
      ""
    }
    statements.mkString("\n")
  }
}
