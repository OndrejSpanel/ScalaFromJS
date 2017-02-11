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

  def nodeToString(n: AST_Node, input: String): String = {
    val b = new StringBuilder
    output(n, input, x => b append x)
    b.toString()
  }

  def output(n: AST_Node, input: String, out: String => Unit): Unit = {
    val source = input.slice(n.start.pos, n.end.endpos)
    // http://lisperator.net/uglifyjs/ast
    // listed in reverse order, so that most specific classes match first
    //noinspection ScalaUnusedSymbol
    def outputDefinitions(decl: String, tn: AST_Definitions) = {
      tn.definitions.foreach { v =>
        out(decl + " ")
        output(v, input, out)
        out("\n")
      }
    }

    n match {
      case tn: AST_True => out("true")
      case tn: AST_False => out("false")
      //case tn: AST_Boolean => "AST_Boolean"
      case tn: AST_Infinity => out("Double.PositiveInfinity")
      case tn: AST_Hole => out("null")
      case tn: AST_Undefined => out("null")
      case tn: AST_NaN => out("Double.NaN")
      case tn: AST_Null => out("null")
      //case tn: AST_Atom => "AST_Atom"
      case tn: AST_RegExp => out("\"" + tn.value + "\".r")
      case tn: AST_Number => out(tn.value.toString)
      case tn: AST_String => out("\"" + tn.value + "\"")
      //case tn: AST_Constant => "AST_Constant"
      case tn: AST_This => out("this") // TODO: handle differences between Scala and JS this
      //case tn: AST_LabelRef => out("AST_LabelRef")
      //case tn: AST_SymbolRef => out("AST_SymbolRef")
      //case tn: AST_Label => out("AST_Label")
      //case tn: AST_SymbolCatch => out("AST_SymbolCatch")
      //case tn: AST_SymbolLambda => out("AST_SymbolLambda")
      //case tn: AST_SymbolDefun => out("AST_SymbolDefun")
      //case tn: AST_SymbolConst => out("AST_SymbolConst")
      //case tn: AST_SymbolFunarg => out(tn.name)
      //case tn: AST_SymbolVar => out("AST_SymbolVar")
      //case tn: AST_SymbolDeclaration => out(tn.name)
      //case tn: AST_SymbolAccessor => out("AST_SymbolAccessor")
      case tn: AST_Symbol => out(tn.name)
      case tn: AST_ObjectGetter => out("AST_ObjectGetter")
      case tn: AST_ObjectSetter => out("AST_ObjectSetter")
      case tn: AST_ObjectKeyVal => out("AST_ObjectKeyVal")
      case tn: AST_ObjectProperty => out("AST_ObjectProperty")
      case tn: AST_Object =>
        tn.properties.foreach { p =>
          out(p.key.toString + " = ")
          output(p.value, input, out)
          out("\n")
        }
      case tn: AST_Array => out("AST_Array")
      case tn: AST_Conditional => out("AST_Conditional")
      case tn: AST_Assign => out("AST_Assign")
      case tn: AST_Binary => out("AST_Binary")
      case tn: AST_UnaryPostfix => out("AST_UnaryPostfix")
      case tn: AST_UnaryPrefix => out("AST_UnaryPrefix")
      case tn: AST_Unary => out("AST_Unary")
      case tn: AST_Sub => out("AST_Sub")
      case tn: AST_Dot => out("AST_Dot")
      case tn: AST_PropAccess => out("AST_PropAccess")
      case tn: AST_Seq => out("AST_Seq")
      case tn: AST_New => out("AST_New")
      case tn: AST_Call => out("AST_Call")
      case tn: AST_VarDef =>
        output(tn.name, input, out)
        tn.value.foreach { v =>
          out(" = ")
          output(v, input, out)
        }
      case tn: AST_Const =>
        outputDefinitions("val", tn)
      case tn: AST_Var =>
        outputDefinitions("var", tn)
      case tn: AST_Definitions => out("AST_Definitions")
      case tn: AST_Continue => out("AST_Continue")
      case tn: AST_Break => out("AST_Break")
      case tn: AST_LoopControl => out("AST_LoopControl")
      case tn: AST_Throw => out("AST_Throw")
      case tn: AST_Return => out("AST_Return")
      case tn: AST_Exit => out("AST_Exit")
      case tn: AST_Jump => out("AST_Jump")
      case tn: AST_If => out("AST_If")
      case tn: AST_With => out("AST_With")
      case tn: AST_ForIn => out("AST_ForIn")
      case tn: AST_For => out("AST_For")
      case tn: AST_While => out("AST_While")
      case tn: AST_Do => out("AST_Do")
      case tn: AST_DWLoop => out("AST_DWLoop")
      case tn: AST_IterationStatement => out("AST_IterationStatement")
      case tn: AST_LabeledStatement => out("AST_LabeledStatement")
      case tn: AST_StatementWithBody => out("AST_StatementWithBody")
      case tn: AST_EmptyStatement => out("AST_EmptyStatement")
      case tn: AST_Finally => out("AST_Finally")
      case tn: AST_Catch => out("AST_Catch")
      case tn: AST_Try => out("AST_Try")
      case tn: AST_Case => out("AST_Case")
      case tn: AST_Default => out("AST_Default")
      case tn: AST_SwitchBranch => out("AST_SwitchBranch")
      case tn: AST_Switch => out("AST_Switch")
      case tn: AST_Defun =>
        out("def ")
        tn.name.foreach(n => output(n, input, out))
        out("(")
        tn.argnames.foreach { arg =>
          output(arg, input, out)
          // TODO: parameter types
          out(": Any")
        }
        out("): Any = \n")
        outputBlock(tn.body, input, out)
      case tn: AST_Function => out("AST_Function")
      case tn: AST_Accessor => out("AST_Accessor")
      case tn: AST_Lambda => out("AST_Lambda")
      case tn: AST_Toplevel => out("AST_Toplevel")
      case tn: AST_Scope => out("AST_Scope")
      case tn: AST_BlockStatement => out("AST_BlockStatement")
      case tn: AST_Block => out("AST_Block")
      case tn: AST_SimpleStatement => out("AST_SimpleStatement")
      case tn: AST_Directive => out("AST_Directive")
      case tn: AST_Debugger => out("AST_Debugger")
    }
  }

  private def outputBlock(body: js.Array[AST_Statement], input: String, out: String => Unit): Unit = {
    for (s <- body) {
      output(s, input, out)
      out("\n")
    }
  }

  def output(ast: AST_Block, input: String): String = {
    val ret = new StringBuilder
    outputBlock(ast.body, input, x => ret.append(x))
    ret.toString()
  }
}
