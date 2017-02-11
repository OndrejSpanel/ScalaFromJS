package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {
  private def nodeClassName(n: AST_Node): String = {
    val nd = n.asInstanceOf[js.Dynamic]
    val s = nd.constructor.name.asInstanceOf[String]
    if (s == "AST_Node" && nd.CTOR != null) {
      nd.CTOR.name.asInstanceOf[String]
    } else s
  }

  def output(ast: AST_Node, input: String): String = {
    val source = input.slice(ast.start.pos, ast.end.endpos)
    "<" + nodeClassName(ast)+ ":" + source + ">"
  }

  def output(ast: AST_Block, input: String): String = {
    val statements = for (s <- ast.body) yield {
      s match {
        case s: AST_SimpleStatement =>
          output(s.body, input)
        case s: AST_Block =>
          "{\n" + output(s, input) + "}\n" // TODO: autoindent
        case s: AST_EmptyStatement =>
          ""
        case s: AST_StatementWithBody =>
          output(s.body, input)
        case _ =>
          val source = input.slice(ast.start.pos, ast.end.endpos)
          "<" + nodeClassName(ast)+ ":" + source + ">"
          //output(s, input)
      }
    }
    statements.mkString("\n")
  }
}
