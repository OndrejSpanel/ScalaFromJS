package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {

  implicit class NonNull[T](val undef: js.UndefOr[T])(implicit ev: Null <:< T) {
    def nonNull: Option[T] = Option[T](undef.orNull)
  }

  private def nodeClassName(n: AST_Node): String = {
    val nd = n.asInstanceOf[js.Dynamic]
    val s = nd.constructor.name.asInstanceOf[String]
    if (s == "AST_Node" && nd.CTOR != null) {
      nd.CTOR.name.asInstanceOf[String]
    } else s
  }

  private def printlnNode(n: js.UndefOr[AST_Node]) = {
    println(n.nonNull.map(n => nodeClassName(n) + ":" + nodeToString(n, "")).getOrElse(""))
  }
  object ForRange {

    def unapply(arg: AST_For): Option[(String, AST_Node, AST_Node, Double)] = {
      (arg.init.nonNull, arg.condition.nonNull, arg.step.nonNull) match {
        case (Some(vv: AST_Var), Some(c: AST_Binary), Some(s: AST_Unary))
          if vv.definitions.length == 1 && c.operator == "<" && s.operator == "++" =>
          val v = vv.definitions(0)
          val n = v.name.name
          (c.left, s.expression) match {
            case (l: AST_SymbolRef, s: AST_SymbolRef) if l.name == n && s.name == n =>
              Some((n, v.value.get, c.right, 1.0))
            case _ => None
          }
        case _ => None
      }
    }
  }

  def nodeToString(n: AST_Node, input: String): String = {
    val b = new StringBuilder
    nodeToOut(n, input, x => b append x)
    b.toString()
  }

  def nodeToOut(n: AST_Node, input: String, out: String => Unit): Unit = {
    def source = input.slice(n.start.pos, n.end.endpos)
    // http://lisperator.net/uglifyjs/ast
    // listed in reverse order, so that most specific classes match first
    //noinspection ScalaUnusedSymbol
    def outputDefinitions(decl: String, tn: AST_Definitions) = {
      tn.definitions.foreach { v =>
        out(decl + " ")
        nodeToOut(v, input, out)
        out("\n")
      }
    }

    def outputArgNames(tn: AST_Lambda) = {
      out("(")
      out(
        tn.argnames.map { arg =>
          nodeToString(arg, input) + ": Any"
        }.mkString(", ")
      )
      out(")")
    }

    def outputCall(tn: AST_Call) = {
      nodeToOut(tn.expression, input, out)
      out("(")
      out(tn.args.map(a => nodeToString(a, input)).mkString(", "))
      out(")")
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
          nodeToOut(p.value, input, out)
          out("\n")
        }
      case tn: AST_Array => out("AST_Array")
      case tn: AST_Conditional =>
        out("if (")
        nodeToOut(tn.condition, input, out)
        out(")")
        nodeToOut(tn.consequent, input, out)
        out(" else ")
        nodeToOut(tn.alternative, input, out)
      //case tn: AST_Assign => out("AST_Assign")
      case tn: AST_Binary =>
        nodeToOut(tn.left, input, out)
        out(" " + tn.operator + " ")
        nodeToOut(tn.right, input, out)
      case tn: AST_Unary =>
        val adjusted = tn.operator match {
          case "++" => Some(" += 1") // TODO: handle return value - beware of AST_UnaryPrefix / AST_UnaryPostfix
          case "--" => Some(" -= 1")
          case _ => None
        }
        adjusted.fold{
          tn match {
            case _: AST_UnaryPrefix =>
              out(tn.operator)
              nodeToOut(tn.expression, input, out)
            case _: AST_UnaryPostfix =>
              nodeToOut(tn.expression, input, out)
              out(tn.operator)
          }
        } { a =>
          nodeToOut(tn.expression, input, out)
          out(a)
        }
      case tn: AST_Sub =>
        nodeToOut(tn.expression, input, out)
        out("(")
        nodeToOut(tn.property, input, out)
        out(")")
      case tn: AST_Dot =>
        nodeToOut(tn.expression, input, out)
        out(".")
        out(tn.property)
      //case tn: AST_PropAccess => out("AST_PropAccess")
      case tn: AST_Seq =>
        nodeToOut(tn.car, input, out)
        out("\n")
        nodeToOut(tn.cdr, input, out)
      case tn: AST_New =>
        out("new ")
        outputCall(tn)
      case tn: AST_Call =>
        outputCall(tn)
      case tn: AST_VarDef =>
        nodeToOut(tn.name, input, out)
        tn.value.nonNull.foreach { v =>
          out(" = ")
          nodeToOut(v, input, out)
        }
      case tn: AST_Const =>
        outputDefinitions("val", tn)
      case tn: AST_Var =>
        outputDefinitions("var", tn)
      case tn: AST_Definitions => out("AST_Definitions")
      case tn: AST_Continue => out("AST_Continue")
      case tn: AST_Break => out("AST_Break")
      case tn: AST_LoopControl => out("AST_LoopControl")
      case tn: AST_Throw =>
        out("throw")
        tn.value.nonNull.foreach { v =>
          out(" ")
          nodeToOut(v, input, out)
          out("\n")
        }
      case tn: AST_Return =>
        out("return")
        tn.value.nonNull.foreach { v =>
          out(" ") // TODO: remove return in trivial cases
          nodeToOut(v, input, out)
          out("\n")
        }
      //case tn: AST_Exit => out("AST_Exit")
      //case tn: AST_Jump => out("AST_Jump")
      case tn: AST_If =>
        out("if (")
        val exp = tn.condition.nonNull.fold(out("xxx"))(nodeToOut(_, input, out))
        out(") ")
        nodeToOut(tn.body, input, out)
        tn.alternative.nonNull.foreach { a =>
          out("else ")
          nodeToOut(a, input, out)
        }
      case tn: AST_With => out("AST_With")
      case tn: AST_ForIn => out("AST_ForIn")
      case tn: AST_For =>
        // TODO: handle a common special cases like for (var x = x0; x < x1; x++)
        tn match {
          case ForRange(name, init, end, step) =>
            val stepString = if (step !=1) s" by $step" else ""
            out(s"for ($name <- ${nodeToString(init,input)} until ${nodeToString(end,input)}$stepString) ")
            nodeToOut(tn.body, input, out)
          case _ => // generic solution using while - reliable, but ugly
            // new scope never needed in classical JS, all variables exists on a function scope
            //out("\n\n{\n")
            tn.init.nonNull.foreach { init =>
              nodeToOut(init, input, out)
            }
            out("while (")
            tn.condition.nonNull.fold(out("true")) { c =>
              nodeToOut(c, input, out)
            }
            out(") {\n")
            nodeToOut(tn.body, input, out)
            tn.step.nonNull.fold(out("true")) { c =>
              nodeToOut(c, input, out)
            }
            out("}\n")
            //out("}\n")
        }

      case tn: AST_While =>
        out(" while (")
        nodeToOut(tn.condition, input, out)
        out(") ")
        nodeToOut(tn.body, input, out)
      case tn: AST_Do =>
        out("do ")
        nodeToOut(tn.body, input, out)
        out(" while (")
        nodeToOut(tn.condition, input, out)
        out(")\n")
      //case tn: AST_DWLoop => out("AST_DWLoop")
      //case tn: AST_IterationStatement => out("AST_IterationStatement")
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
        tn.name.nonNull.foreach(n => nodeToOut(n, input, out))
        outputArgNames(tn)
        out(" = ") // TODO: single statement without braces
        out("{\n") // TODO: autoindent
        blockToOut(tn.body, input, out)
        out("}\n")
      case tn: AST_Function =>
        outputArgNames(tn)
        out(" = ")
        out("{\n") // TODO: autoindent
        blockToOut(tn.body, input, out)
        out("}\n")
      case tn: AST_Accessor => out("AST_Accessor")
      case tn: AST_Lambda => out("AST_Lambda")
      //case tn: AST_Toplevel => out("AST_Toplevel")
      //case tn: AST_Scope => out("AST_Scope")
      case tn: AST_Block =>
        out("{\n") // TODO: autoindent
        blockToOut(tn.body, input, out)
        out("}\n")
      //case tn: AST_BlockStatement =>
      case tn: AST_SimpleStatement =>
        nodeToOut(tn.body, input, out)
        out("\n")
      case tn: AST_Directive => out("AST_Directive")
      case tn: AST_Debugger => out("AST_Debugger")
    }
  }

  private def blockToOut(body: js.Array[AST_Statement], input: String, out: String => Unit): Unit = {
    for (s <- body) {
      nodeToOut(s, input, out)
    }
  }

  def output(ast: AST_Block, input: String): String = {
    val ret = new StringBuilder
    blockToOut(ast.body, input, x => ret.append(x))
    ret.toString()
  }
}
