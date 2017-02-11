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

  def output(n: AST_Node, input: String, out: String => Unit): Unit = {
    val source = input.slice(n.start.pos, n.end.endpos)

    val s = n match {
      case _: AST_True => "AST_True"
      case _: AST_False => "AST_False"
      case _: AST_Boolean => "AST_Boolean"
      case _: AST_Infinity => "AST_Infinity"
      case _: AST_Hole => "AST_Hole"
      case _: AST_Undefined => "AST_Undefined"
      case _: AST_NaN => "AST_NaN"
      case _: AST_Null => "AST_Null"
      case _: AST_Atom => "AST_Atom"
      case _: AST_RegExp => "AST_RegExp"
      case _: AST_Number => "AST_Number"
      case _: AST_String => "AST_String"
      case _: AST_Constant => "AST_Constant"
      case _: AST_This => "AST_This"
      case _: AST_LabelRef => "AST_LabelRef"
      case _: AST_SymbolRef => "AST_SymbolRef"
      case _: AST_Label => "AST_Label"
      case _: AST_SymbolCatch => "AST_SymbolCatch"
      case _: AST_SymbolLambda => "AST_SymbolLambda"
      case _: AST_SymbolDefun => "AST_SymbolDefun"
      case _: AST_SymbolConst => "AST_SymbolConst"
      case _: AST_SymbolFunarg => "AST_SymbolFunarg"
      case _: AST_SymbolVar => "AST_SymbolVar"
      case _: AST_SymbolDeclaration => "AST_SymbolDeclaration"
      case _: AST_SymbolAccessor => "AST_SymbolAccessor"
      case _: AST_Symbol => "AST_Symbol"
      case _: AST_ObjectGetter => "AST_ObjectGetter"
      case _: AST_ObjectSetter => "AST_ObjectSetter"
      case _: AST_ObjectKeyVal => "AST_ObjectKeyVal"
      case _: AST_ObjectProperty => "AST_ObjectProperty"
      case _: AST_Object => "AST_Object"
      case _: AST_Array => "AST_Array"
      case _: AST_Conditional => "AST_Conditional"
      case _: AST_Assign => "AST_Assign"
      case _: AST_Binary => "AST_Binary"
      case _: AST_UnaryPostfix => "AST_UnaryPostfix"
      case _: AST_UnaryPrefix => "AST_UnaryPrefix"
      case _: AST_Unary => "AST_Unary"
      case _: AST_Sub => "AST_Sub"
      case _: AST_Dot => "AST_Dot"
      case _: AST_PropAccess => "AST_PropAccess"
      case _: AST_Seq => "AST_Seq"
      case _: AST_New => "AST_New"
      case _: AST_Call => "AST_Call"
      case _: AST_VarDef => "AST_VarDef"
      case _: AST_Const => "AST_Const"
      case _: AST_Var => "AST_Var"
      case _: AST_Definitions => "AST_Definitions"
      case _: AST_Continue => "AST_Continue"
      case _: AST_Break => "AST_Break"
      case _: AST_LoopControl => "AST_LoopControl"
      case _: AST_Throw => "AST_Throw"
      case _: AST_Return => "AST_Return"
      case _: AST_Exit => "AST_Exit"
      case _: AST_Jump => "AST_Jump"
      case _: AST_If => "AST_If"
      case _: AST_With => "AST_With"
      case _: AST_ForIn => "AST_ForIn"
      case _: AST_For => "AST_For"
      case _: AST_While => "AST_While"
      case _: AST_Do => "AST_Do"
      case _: AST_DWLoop => "AST_DWLoop"
      case _: AST_IterationStatement => "AST_IterationStatement"
      case _: AST_LabeledStatement => "AST_LabeledStatement"
      case _: AST_StatementWithBody => "AST_StatementWithBody"
      case _: AST_EmptyStatement => "AST_EmptyStatement"
      case _: AST_Finally => "AST_Finally"
      case _: AST_Catch => "AST_Catch"
      case _: AST_Try => "AST_Try"
      case _: AST_Case => "AST_Case"
      case _: AST_Default => "AST_Default"
      case _: AST_SwitchBranch => "AST_SwitchBranch"
      case _: AST_Switch => "AST_Switch"
      case _: AST_Defun => "AST_Defun"
      case _: AST_Function => "AST_Function"
      case _: AST_Accessor => "AST_Accessor"
      case _: AST_Lambda => "AST_Lambda"
      case _: AST_Toplevel => "AST_Toplevel"
      case _: AST_Scope => "AST_Scope"
      case _: AST_BlockStatement => "AST_BlockStatement"
      case _: AST_Block => "AST_Block"
      case _: AST_SimpleStatement => "AST_SimpleStatement"
      case _: AST_Directive => "AST_Directive"
      case _: AST_Debugger => "AST_Debugger"
    }

    out("<" + s + ":" + source + ">")
  }

  private def outputRecurse(ast: AST_Block, input: String, out: String => Unit): Unit = {
    for (s <- ast.body) yield {
      output(s, input, out)
    }
  }

  def output(ast: AST_Block, input: String): String = {
    var ret = new StringBuilder
    outputRecurse(ast, input, x => ret.append(x))
    ret.toString()
  }
}
