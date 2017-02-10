package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {
  private def nodeClassName(n: AST_Node): String = {
    n.asInstanceOf[js.Dynamic].constructor.name.asInstanceOf[String]
  }

  def output(ast: AST_Node, input: String): String = {
    val source = input.slice(ast.start.pos, ast.end.endpos)
    "<" + nodeClassName(ast)+ ":" + source + ">"
  }

  def output(ast: AST_Block, input: String): String = {
    val statements = for (s <- ast.body) yield {
      s match {
        case s: AST_SimpleStatement =>
          "<ss>" + output(s.body, input)
        case s: AST_Block =>
          "<block>{\n" + output(s, input) + "}\n" // TODO: autoindent
        case s: AST_EmptyStatement =>
          "<empty>"
        case s: AST_StatementWithBody =>
          "<sb>" + output(s.body, input)
        case _ =>
          val source = input.slice(ast.start.pos, ast.end.endpos)
          "<" + nodeClassName(ast)+ ":" + source + ">"
          //output(s, input)
      }
    }
    statements.mkString("\n")
  }
}
