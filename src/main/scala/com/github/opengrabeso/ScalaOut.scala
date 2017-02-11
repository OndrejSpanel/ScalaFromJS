package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {
  private def nodeClassName(n: AST_Node): String = {
    val s = n.asInstanceOf[js.Dynamic].constructor.name.asInstanceOf[String]
    if (s == "AST_Node") {
      n match {
        case _ : AST_Statement => "AST_Statement"
        case _ : AST_VarDef => "AST_VarDef"
        case _ : AST_Call => "AST_Call"
        case _ : AST_Seq => "AST_Seq"
        case _ : AST_PropAccess => "AST_PropAccess"
        case _ : AST_Unary => "AST_Unary"
        case _ : AST_Binary => "AST_Binary"
        case _ : AST_Conditional => "AST_Conditional"
        case _ : AST_Array => "AST_Array"
        case _ : AST_Object => "AST_Object"
        case _ : AST_ObjectProperty => "AST_ObjectProperty"
        case _ : AST_Symbol => "AST_Symbol"
        case _ : AST_Constant => "AST_Constant"
      }
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
