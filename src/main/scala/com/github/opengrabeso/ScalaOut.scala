package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {

  class Config {
    // annotate unknown constructs with a comment (source is always passed through)
    var unknowns = true
  }

  object Config {
    val default = new Config
  }

  case class InputContext(input: String)

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

  private def printlnNode(n: js.UndefOr[AST_Node])(implicit outConfig: Config, input: InputContext) = {
    println(n.nonNull.map(n => nodeClassName(n) + ":" + nodeToString(n)).getOrElse(""))
  }

  // exctactor for special cases of the for loop
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

  def nodeToString(n: AST_Node)(implicit outConfig: Config, input: InputContext): String = {
    val b = new StringBuilder
    nodeToOut(n)(outConfig, input, x => b append x)
    b.toString()
  }

  def nodeToOut(n: AST_Node)(implicit outConfig: Config, input: InputContext, out: String => Unit): Unit = {
    def source = input.input.slice(n.start.pos, n.end.endpos)
    // http://lisperator.net/uglifyjs/ast
    // listed in reverse order, so that most specific classes match first
    //noinspection ScalaUnusedSymbol
    def outputDefinitions(decl: String, tn: AST_Definitions) = {
      tn.definitions.foreach { v =>
        out(decl + " ")
        nodeToOut(v)
        out("\n")
      }
    }

    def outputArgNames(tn: AST_Lambda) = {
      out("(")
      out(
        tn.argnames.map { arg =>
          nodeToString(arg) + ": Any"
        }.mkString(", ")
      )
      out(")")
    }

    def outputCall(tn: AST_Call) = {
      nodeToOut(tn.expression)
      out("(")
      out(tn.args.map(nodeToString).mkString(", "))
      out(")")
    }

    def outputUnknownNode(tn: AST_Node, statement: Boolean = false) = {
      def shortNodeClassName(n: String) = {
        val prefix = "AST_"
        if (n startsWith prefix) n.drop(prefix.length)
        else n
      }
      if (outConfig.unknowns) {
        out("/* " + shortNodeClassName(nodeClassName(tn)) + " */ ")
      }
      out(source)
      if (statement) out("\n")
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
      case tn: AST_ObjectGetter => outputUnknownNode(n)
      case tn: AST_ObjectSetter => outputUnknownNode(tn)
      case tn: AST_ObjectKeyVal => outputUnknownNode(tn)
      case tn: AST_ObjectProperty => outputUnknownNode(tn)
      case tn: AST_Object =>
        tn.properties.foreach { p =>
          out(p.key.toString + " = ")
          nodeToOut(p.value)
          out("\n")
        }
      case tn: AST_Array => outputUnknownNode(tn)
      case tn: AST_Conditional =>
        out("if (")
        nodeToOut(tn.condition)
        out(")")
        nodeToOut(tn.consequent)
        out(" else ")
        nodeToOut(tn.alternative)
      //case tn: AST_Assign => outputUnknownNode(tn)
      case tn: AST_Binary =>
        nodeToOut(tn.left)
        out(" " + tn.operator + " ")
        nodeToOut(tn.right)
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
              nodeToOut(tn.expression)
            case _: AST_UnaryPostfix =>
              nodeToOut(tn.expression)
              out(tn.operator)
          }
        } { a =>
          nodeToOut(tn.expression)
          out(a)
        }
      case tn: AST_Sub =>
        nodeToOut(tn.expression)
        out("(")
        nodeToOut(tn.property)
        out(")")
      case tn: AST_Dot =>
        nodeToOut(tn.expression)
        out(".")
        out(tn.property)
      //case tn: AST_PropAccess => outputUnknownNode(tn)
      case tn: AST_Seq =>
        nodeToOut(tn.car)
        out("\n")
        nodeToOut(tn.cdr)
      case tn: AST_New =>
        out("new ")
        outputCall(tn)
      case tn: AST_Call =>
        outputCall(tn)
      case tn: AST_VarDef =>
        nodeToOut(tn.name)
        tn.value.nonNull.foreach { v =>
          out(" = ")
          nodeToOut(v)
        }
      case tn: AST_Const =>
        outputDefinitions("val", tn)
      case tn: AST_Var =>
        outputDefinitions("var", tn)
      //case tn: AST_Definitions => outputUnknownNode(tn)
      case tn: AST_Continue => outputUnknownNode(tn, true)
      case tn: AST_Break => outputUnknownNode(tn, true)
      //case tn: AST_LoopControl => outputUnknownNode(tn)
      case tn: AST_Throw =>
        out("throw")
        tn.value.nonNull.foreach { v =>
          out(" ")
          nodeToOut(v)
          out("\n")
        }
      case tn: AST_Return =>
        out("return")
        tn.value.nonNull.foreach { v =>
          out(" ") // TODO: remove return in trivial cases
          nodeToOut(v)
          out("\n")
        }
      //case tn: AST_Exit => outputUnknownNode(tn)
      //case tn: AST_Jump => outputUnknownNode(tn)
      case tn: AST_If =>
        out("if (")
        val exp = tn.condition.nonNull.fold(out("xxx"))(nodeToOut(_))
        out(") ")
        nodeToOut(tn.body)
        tn.alternative.nonNull.foreach { a =>
          out("else ")
          nodeToOut(a)
        }
      case tn: AST_With => outputUnknownNode(tn, true)
      case tn: AST_ForIn => outputUnknownNode(tn, true)
      case tn: AST_For =>
        // TODO: handle a common special cases like for (var x = x0; x < x1; x++)
        tn match {
          case ForRange(name, init, end, step) =>
            val stepString = if (step !=1) s" by $step" else ""
            out(s"for ($name <- ${nodeToString(init)} until ${nodeToString(end)}$stepString) ")
            nodeToOut(tn.body)
          case _ => // generic solution using while - reliable, but ugly
            // new scope never needed in classical JS, all variables exists on a function scope
            //out("\n\n{\n")
            tn.init.nonNull.foreach { init =>
              nodeToOut(init)
            }
            out("while (")
            tn.condition.nonNull.fold(out("true")) { c =>
              nodeToOut(c)
            }
            out(") {\n")
            nodeToOut(tn.body)
            tn.step.nonNull.fold(out("true")) { c =>
              nodeToOut(c)
            }
            out("}\n")
            //out("}\n")
        }

      case tn: AST_While =>
        out(" while (")
        nodeToOut(tn.condition)
        out(") ")
        nodeToOut(tn.body)
      case tn: AST_Do =>
        out("do ")
        nodeToOut(tn.body)
        out(" while (")
        nodeToOut(tn.condition)
        out(")\n")
      //case tn: AST_DWLoop => outputUnknownNode(tn)
      //case tn: AST_IterationStatement => outputUnknownNode(tn)
      case tn: AST_LabeledStatement =>
        out(s"/* label ${nodeToString(tn.label)} */")
        nodeToOut(tn.body)
      //case tn: AST_StatementWithBody => outputUnknownNode(tn)
      case tn: AST_EmptyStatement =>
      case tn: AST_Finally => outputUnknownNode(tn)
      case tn: AST_Catch => outputUnknownNode(tn)
      case tn: AST_Try => outputUnknownNode(tn)
      case tn: AST_Case => outputUnknownNode(tn)
      case tn: AST_Default => outputUnknownNode(tn)
      case tn: AST_SwitchBranch => outputUnknownNode(tn)
      case tn: AST_Switch => outputUnknownNode(tn)
      case tn: AST_Defun =>
        out("def ")
        tn.name.nonNull.foreach(n => nodeToOut(n))
        outputArgNames(tn)
        out(" = ") // TODO: single statement without braces
        out("{\n") // TODO: autoindent
        blockToOut(tn.body)
        out("}\n")
      case tn: AST_Function =>
        outputArgNames(tn)
        out(" = ")
        out("{\n") // TODO: autoindent
        blockToOut(tn.body)
        out("}\n")
      case tn: AST_Accessor => outputUnknownNode(tn)
      case tn: AST_Lambda => outputUnknownNode(tn)
      //case tn: AST_Toplevel => outputUnknownNode(tn)
      //case tn: AST_Scope => outputUnknownNode(tn)
      case tn: AST_Block =>
        out("{\n") // TODO: autoindent
        blockToOut(tn.body)
        out("}\n")
      //case tn: AST_BlockStatement =>
      case tn: AST_SimpleStatement =>
        nodeToOut(tn.body)
        out("\n")
      case tn: AST_Directive => outputUnknownNode(tn)
      case tn: AST_Debugger => outputUnknownNode(tn)
    }
  }

  private def blockToOut(body: js.Array[AST_Statement])(implicit outConfig: Config, input: InputContext, out: String => Unit): Unit = {
    for (s <- body) {
      nodeToOut(s)
    }
  }

  def output(ast: AST_Block, input: String, outConfig: Config = Config.default): String = {
    val ret = new StringBuilder
    val inputContext = InputContext(input)
    blockToOut(ast.body)(outConfig, inputContext, x => ret.append(x))
    ret.toString()
  }

}
