package com.github.opengrabeso

import com.github.opengrabeso.Uglify._

import scala.scalajs.js

object ScalaOut {

  class Config {
    // annotate unknown constructs with a comment (source is always passed through)
    var unknowns = true
  }

  abstract class Output extends ((String) => Unit) {
    def out(x: String): Unit

    def eol(num: Int = 1): Unit = out("\n")

    def changeIndent(ch: Int): Unit = ()

    def indent(): Unit = changeIndent(+1)
    def unindent(): Unit = changeIndent(-1)
  }

  abstract class NiceOutput extends Output {
    def out(x: String): Unit

    private var eolDone = Int.MaxValue
    private var indentLevel = 0

    override def changeIndent(ch: Int): Unit = indentLevel += ch

    private def singleLine(line: String) = {
      if (eolDone > 0) out(" " * (indentLevel * 2))
      out(line)
      if (line == "\n") eolDone += 1
      else eolDone = if (line.lastOption.contains('\n')) 1 else 0
    }

    override def eol(num: Int) = {
      while (eolDone < num) {
        out("\n")
        eolDone += 1
      }
    }

    def apply(v: String) = {
      val lines = v.linesWithSeparators
      for (line <- lines) {
        singleLine(line)
      }

    }

  }

  object Config {
    val default = new Config
  }

  case class InputContext(input: String) {
    var commentsDumped = Set.empty[Int]
  }

  implicit class NonNull[T](val undef: js.UndefOr[T])(implicit ev: Null <:< T) {
    def nonNull: Option[T] = Option[T](undef.orNull)
  }

  def markEnd[T](seq: Seq[T]) = seq zip (seq.drop(1).map(x => true) :+ false)


  private def nodeClassName(n: AST_Node): String = {
    val nd = n.asInstanceOf[js.Dynamic]
    val s = nd.constructor.name.asInstanceOf[String]
    if (s == "AST_Node" && nd.CTOR != null) {
      nd.CTOR.name.asInstanceOf[String]
    } else s
  }

  //noinspection ScalaUnusedSymbol
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


  class OutToString extends Output {
    val b = new StringBuilder
    def out(x: String) = b append x
    // no smart eol handling - string will be processed when doing proper output
    override def eol(num: Int) = out("\n")
    override def apply(x: String) = out(x)
    def result = b.toString
  }
  def nodeToString(n: AST_Node)(implicit outConfig: Config, input: InputContext): String = {
    val b = new OutToString
    nodeToOut(n)(outConfig, input, b)
    b.result
  }

  def dumpComments(n: AST_Node)(implicit outConfig: Config, input: InputContext, out: Output) = {
    // start is mostly defined, but not for accessor
    for {
      start <- n.start.nonNull
      c <- start.comments_before
    } {
      if (!(input.commentsDumped contains c.pos)) {
        if (c.`type` == "comment2") {
          out("/*")
          out(c.value.toString)
          out("*/")
          out.eol()
        } else {
          out("//")
          out(c.value.toString)
          out.eol()
        }

        input.commentsDumped += c.pos
      }
    }
  }

  def nodeToOut(n: AST_Node)(implicit outConfig: Config, input: InputContext, out: Output): Unit = {
    def source = (for {
      s <- n.start
      e <- n.end
    } yield input.input.slice(s.pos, e.endpos)).getOrElse("")
    // http://lisperator.net/uglifyjs/ast
    // listed in reverse order, so that most specific classes match first
    //noinspection ScalaUnusedSymbol
    def outputDefinitions(decl: String, tn: AST_Definitions) = {
      tn.definitions.foreach { v =>
        out(decl + " ")
        nodeToOut(v)
        out.eol()
      }
    }

    def outputNodes[T](ns: Seq[T])(outOne: T => Unit, delimiter: String = ", ") = {
      for ((arg, delim) <- markEnd(ns)) {
        outOne(arg) + ": Any"
        if (delim) out(delimiter)
      }
    }
    def outputArgNames(tn: AST_Lambda) = {
      out("(")
      outputNodes(tn.argnames) { n =>
        nodeToOut(n)
        out(": Any")
      }
      out(")")
    }

    def outputCall(tn: AST_Call) = {
      nodeToOut(tn.expression)
      out("(")
      outputNodes(tn.args)(nodeToOut)
      out(")")
    }

    def quote (s: String): String = "\"" + escape(s) + "\""
    def escape(s: String): String = s.flatMap(escapedChar)

    def escapedChar(ch: Char): String = ch match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"'  => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
      else              String.valueOf(ch)
    }

    def outputUnknownNode(tn: AST_Node, statement: Boolean = false) = {
      def shortNodeClassName(n: String) = {
        val prefix = "AST_"
        if (n startsWith prefix) n.drop(prefix.length)
        else n
      }
      if (outConfig.unknowns) {
        out("/* Unsupported: " + shortNodeClassName(nodeClassName(tn)) + " */ ")
      }
      out(source)
      if (statement) {
        out.eol()
      }
    }

    dumpComments(n)

    //noinspection ScalaUnusedSymbol
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
      case tn: AST_String => out(quote(tn.value))
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
      case tn: AST_Symbol =>
        // TODO: other rules needed?
        if (Keywords(tn.name)) {
          out("`" + tn.name + "`")
        } else out(tn.name)
      case tn: AST_ObjectGetter =>
        out("def ")
        nodeToOut(tn.key)
        nodeToOut(tn.value)
        out.eol()
      case tn: AST_ObjectSetter =>
        out("def ")
        nodeToOut(tn.key)
        nodeToOut(tn.value)
        out.eol()
      case tn: AST_ObjectKeyVal =>
          out(tn.key + " = ")
          nodeToOut(tn.value)
          out.eol()
      case tn: AST_ObjectProperty =>
      case tn: AST_Object =>
        out("js.Dynamic.literal {\n")
        out.indent()
        tn.properties.foreach(nodeToOut)
        out.unindent()
        out("}")
      case tn: AST_Array =>
        out("Array(")
        outputNodes(tn.elements)(nodeToOut)
        out(")")
      case tn: AST_Conditional =>
        out("if (")
        nodeToOut(tn.condition)
        out(") ")
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
        out.eol()
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
          out.eol()
        }
      case tn: AST_Return =>
        out("return")
        tn.value.nonNull.foreach { v =>
          out(" ") // TODO: remove return in trivial cases
          nodeToOut(v)
          out.eol()
        }
      //case tn: AST_Exit => outputUnknownNode(tn)
      //case tn: AST_Jump => outputUnknownNode(tn)
      case tn: AST_If =>
        out("if (")
        val exp = tn.condition.nonNull.fold(out("xxx"))(nodeToOut)
        out(") ")
        nodeToOut(tn.body)
        tn.alternative.nonNull.foreach { a =>
          out(" else ")
          nodeToOut(a)
        }
      case tn: AST_With => outputUnknownNode(tn, true)
      case tn: AST_ForIn =>
        out("for (")
        tn.name.nonNull.fold(nodeToOut(tn.init))(nodeToOut)
        out(" <- ")
        nodeToOut(tn.`object`)
        out(") ")
        nodeToOut(tn.body)
      case tn: AST_For =>
        // TODO: handle a common special cases like for (var x = x0; x < x1; x++)
        tn match {
          case ForRange(name, init, end, step) =>
            out("for (")
            out(name)
            out(" <- ")
            nodeToOut(init)
            out(" until ")
            nodeToOut(end)
            if (step != 1) out(s" by $step")
            out(") ")
            nodeToOut(tn.body)
          case _ => // generic solution using while - reliable, but ugly
            // new scope never needed in classical JS, all variables exists on a function scope
            //out("\n\n{\n")
            tn.init.nonNull.foreach { init =>
              nodeToOut(init)
            }
            out("while (")
            tn.condition.nonNull.fold(out("true"))(nodeToOut)
            out(") {\n")
            nodeToOut(tn.body)
            tn.step.nonNull.foreach(nodeToOut)
            out.eol()
            out("}\n")
            //out("}\n")
        }

      case tn: AST_While =>
        out("while (")
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
      case tn: AST_Finally =>
        out(" finally ")
        blockBracedToOut(tn.body)
      case tn: AST_Catch =>
        out(" catch {\n")
        out.indent()
        out("case ")
        out.indent()
        nodeToOut(tn.argname)
        out(" =>\n")
        blockToOut(tn.body)
        out.unindent()
        out.unindent()
        out("}\n")
      case tn: AST_Try =>
        out("try ")
        blockBracedToOut(tn.body)
        tn.bcatch.nonNull.foreach(nodeToOut)
        tn.bfinally.nonNull.foreach(nodeToOut)
      case tn: AST_Case =>
        out("case ")
        nodeToOut(tn.expression)
        out(" =>\n")
        out.indent()
        blockToOut(tn.body)
        out.eol()
        out.unindent()
      case tn: AST_Default =>
        out("case _ =>\n")
        out.indent()
        blockToOut(tn.body)
        out.eol()
        out.unindent()
      //case tn: AST_SwitchBranch => outputUnknownNode(tn)
      case tn: AST_Switch =>
        nodeToOut(tn.expression)
        out(" match ")
        blockBracedToOut(tn.body, true)
      case tn: AST_Defun =>
        out.eol(2)
        out("def ")
        tn.name.nonNull.foreach(n => nodeToOut(n))
        outputArgNames(tn)
        out(" = ")
        blockBracedToOut(tn.body)
      case tn: AST_Function =>
        outputArgNames(tn)
        out(" => ")
        blockBracedToOut(tn.body)
      case tn: AST_Accessor =>
        outputArgNames(tn)
        out(" = ")
        blockBracedToOut(tn.body)
      case tn: AST_Lambda => outputUnknownNode(tn)
      //case tn: AST_Toplevel => outputUnknownNode(tn)
      //case tn: AST_Scope => outputUnknownNode(tn)
      case tn: AST_Block =>
        blockBracedToOut(tn.body)
      //case tn: AST_BlockStatement =>
      case tn: AST_SimpleStatement =>
        nodeToOut(tn.body)
        out.eol()
      case tn: AST_Directive =>
        if (source != """"use strict";""") { // east use strict silently
          outputUnknownNode(tn)
          out.eol()
        }
      case tn: AST_Debugger =>
        outputUnknownNode(tn)
        out.eol()
    }
  }

  private def blockBracedToOut(body: js.Array[AST_Statement], force: Boolean = false)(implicit outConfig: Config, input: InputContext, out: Output) = {
    // TODO: single statement without braces
    out("{\n") // TODO: autoindent
    out.indent()
    blockToOut(body)
    out.unindent()
    out.eol()
    out("}")
  }

  private def blockToOut(body: js.Array[AST_Statement])(implicit outConfig: Config, input: InputContext, out: Output): Unit = {
    for ((s, notLast) <- markEnd(body)) {
      nodeToOut(s)
      if (notLast) out.eol()
    }
  }

  def output(ast: AST_Block, input: String, outConfig: Config = Config.default): String = {
    val sb = new StringBuilder
    val ret = new NiceOutput {
      def out(x: String) = sb append x
    }
    val inputContext = InputContext(input)
    blockToOut(ast.body)(outConfig, inputContext, ret)
    sb.result
  }

}
