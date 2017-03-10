package com.github.opengrabeso

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._

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

  case class InputContext(input: String, types: SymbolTypes) {
    var commentsDumped = Set.empty[Int]
  }

  implicit class OutStringContext(val sc: StringContext)(implicit outConfig: Config, input: InputContext, output: Output) {

    def outEx(ex: Any) {
      ex match {
        case s: String =>
          //println("symbol")
          output(s)
        case s: AST_Symbol =>
          //println("symbol")
          identifierToOut(output, s.name)
        case n: AST_Node =>
          nodeToOut(n)
        case x if js.isUndefined(x) =>
        case any =>
          output(any.toString)
      }
    }

    def out(args: Any*): Unit = {
      val strings = sc.parts.iterator
      val expressions = args.iterator

      import StringContext.{treatEscapes=>escape}
      output(escape(strings.next))
      while(strings.hasNext) {
        val ex = expressions.next
        outEx(ex)
        output(escape(strings.next))
      }
      assert(!strings.hasNext)
      assert(!expressions.hasNext)
    }
  }


  def markEnd[T](seq: Seq[T]) = seq zip (seq.drop(1).map(_ => true) :+ false)


  //noinspection ScalaUnusedSymbol
  private def printlnNode(n: js.UndefOr[AST_Node])(implicit outConfig: Config, input: InputContext) = {
    println(n.nonNull.map(n => nodeClassName(n) + ":" + nodeToString(n)).getOrElse(""))
  }

  // exctactor for special cases of the for loop
  object ForRange {
    object VarOrLet {
      def unapply(arg: AST_Definitions): Option[AST_Definitions] = arg match {
        case _: AST_Var => Some(arg)
        case _: AST_Let => Some(arg)
        case _ => None
      }
    }

    def unapply(arg: AST_For): Option[(String, String, AST_Node, AST_Node, AST_Node)] = {
      def negateStep(step: AST_Node): AST_Node = {
        new AST_UnaryPrefix {
          fillTokens(this, step)
          operator = "-"
          expression = step.clone()
        }

      }

      (arg.init.nonNull, arg.condition.nonNull, arg.step.nonNull) match {
        case (Some(VarOrLet(AST_Definitions(v))), Some(AST_Binary(cLeft, rel, cRight)), Some(AST_Binary(expr, assign, step))) =>
          val n = v.name.name
          (cLeft, expr) match {
            case (AST_SymbolRef(`n`, _, _), AST_SymbolRef(`n`, _, _)) =>
              (rel, assign) match {
                case ("<", "+=") =>
                  Some((n, "until", v.value.get, cRight, step))
                case ("<=", "+=") =>
                  Some((n, "to", v.value.get, cRight, step))
                case (">", "-=") =>
                  Some((n, "until", v.value.get, cRight, negateStep(step)))
                case (">=", "-=") =>
                  Some((n, "to", v.value.get, cRight, negateStep(step)))
                case _ =>
                  None
              }
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
          out"/*${c.value}*/\n"
        } else {
          out"//${c.value}\n"
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
    def outputDefinitions(isVal: Boolean, tn: AST_Definitions) = {
      tn.definitions.foreach { v =>
        val decl = if (isVal) "val" else "var"
        out"$decl $v\n"
      }
    }

    def outputNodes[T](ns: Seq[T])(outOne: T => Unit, delimiter: String = ", ") = {
      for ((arg, delim) <- markEnd(ns)) {
        outOne(arg) + ": Any"
        if (delim) out(delimiter)
      }
    }

    def outputArgType(n: AST_SymbolFunarg) = {
      val typeString = n.thedef.nonNull.fold(SymbolTypes.any.toString)(input.types.getAsScala(_))
      out": $typeString"
      for (init <- n.init.nonNull.flatMap(_.headOption)) {
        out" = $init"
      }
    }

    def outputArgNames(tn: AST_Lambda, types: Boolean = false) = {
      out("(")
      outputNodes(tn.argnames) { n =>
        out"$n"
        if (types) {
          outputArgType(n)
        } else {
          val sid = n.thedef.nonNull.flatMap(SymbolTypes.id)
          for (t <- input.types.get(sid)) {
            out": ${SymbolTypes.mapSimpleTypeToScala(t)}"
          }
        }
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
      case tn: AST_RegExp => out""""${tn.value}".r"""
      case tn: AST_Number => out"${tn.value}"
      case tn: AST_String => out(quote(tn.value))
      //case tn: AST_Constant => "AST_Constant"
      case tn: AST_This => out("this") // TODO: handle differences between Scala and JS this
      case tn: AST_Super => out("super")
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
      //case tn: AST_SymbolRef => identifierToOut(out, tn.name)
      case tn: AST_Symbol =>
        identifierToOut(out, tn.name)
      case tn: AST_ObjectGetter =>
        out.eol()
        out"def ${tn.key}${tn.value}\n"
      case tn: AST_ObjectSetter =>
        out.eol()
        out"def ${tn.key}${tn.value}\n"
      case tn: AST_ObjectKeyVal =>
        out"var ${identifier(tn.key)} = ${tn.value}\n"
      //case tn: AST_ObjectProperty =>
      case tn: AST_ConciseMethod =>
        val keyName = tn.key.name /*match {
          case "constructor" => "this"
          case x => x
        }*/
        out"def ${identifier(keyName)}${tn.value}\n"

      case tn: AST_Object =>
        out("new {\n") // prefer anonymous class over js.Dynamic.literal
        out.indent()
        tn.properties.foreach{ n =>
          nodeToOut(n)
          out.eol()
        }
        out.unindent()
        out.eol()
        out("}")
      case tn: AST_Array =>
        out("Array(")
        outputNodes(tn.elements)(nodeToOut)
        out(")")
      case tn: AST_Conditional =>
        out"if (${tn.condition}) ${tn.consequent} else ${tn.alternative}"
      //case tn: AST_Assign => outputUnknownNode(tn)
      case tn: AST_Binary =>
        tn.operator match {
          case "instanceof" =>
            out"${tn.left}.isInstanceOf[${tn.right}]"
          case _ =>
            out"${tn.left} ${tn.operator} ${tn.right}"
        }
      case tn: AST_Unary =>
        tn.operator match {
          case "typeof" =>
            out"${tn.expression}.getClass"
          case _ =>
            tn match {
              case _: AST_UnaryPrefix =>
                out(tn.operator)
                nodeToOut(tn.expression)
              case _: AST_UnaryPostfix =>
                nodeToOut(tn.expression)
                out(tn.operator)
            }
        }
      case tn: AST_Sub =>
        nodeToOut(tn.expression)
        out("(")
        nodeToOut(tn.property)
        out(")")
      case tn: AST_Dot =>
        nodeToOut(tn.expression)
        out(".")
        identifierToOut(out, tn.property)
      //case tn: AST_PropAccess => outputUnknownNode(tn)
      case tn: AST_Seq =>
        out("{\n")
        out.indent()
        nodeToOut(tn.car)
        out.eol()
        // handle a special case - cdr also seq
        def processCdr(cdr: AST_Node): Unit = {
          cdr match {
            case ss: AST_Seq =>
              nodeToOut(ss.car)
              out.eol()
              processCdr(ss.cdr)
            case _ =>
              nodeToOut(cdr)
          }
        }
        processCdr(tn.cdr)
        out.unindent()
        out.eol()
        out("}")
      case tn: AST_New =>
        out("new ")
        outputCall(tn)
      case tn: AST_Call =>
        outputCall(tn)
      case tn: AST_VarDef =>
        nodeToOut(tn.name)
        tn.value.nonNull.fold {
          val tpe = tn.name.thedef.nonNull.map(s => input.types.getAsScala(s))
          val typeName = tpe.getOrElse(SymbolTypes.any)
          out": $typeName"
        } { v =>
          out(" = ")
          nodeToOut(v)
        }
      case tn: AST_Const =>
        outputDefinitions(true, tn)
      case tn: AST_Var =>
        outputDefinitions(false, tn) // we assume scoping is reasonable, we do not try to handle hoisting
      case tn: AST_Let =>
        outputDefinitions(false, tn)
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
          out(" ")
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
        tn match {
          case ForRange(name, until, init, end, step) =>
            out"for ($name <- $init $until $end"
            step match {
              case AST_Number(1) =>
              case _ => out" by $step"
            }
            out") ${tn.body}"
          case _ => // generic solution using while - reliable, but ugly
            // new scope never needed in classical JS, all variables exists on a function scope
            val isScoped = tn.init.nonNull match {
              case Some(AST_Let(_*)) => true
              case _ => false
            }
            if (isScoped) {
              out.eol()
              out("{\n")
              out.indent()
            }
            //out("\n\n{\n")
            tn.init.nonNull.foreach { init =>
              nodeToOut(init)
            }
            out.eol()
            out("while (")
            tn.condition.nonNull.fold(out("true"))(nodeToOut)
            out(") {\n")
            out.indent()
            nodeToOut(tn.body)
            out.eol()
            tn.step.nonNull.foreach(nodeToOut)
            out.unindent()
            out.eol()
            out("}\n")
            if (isScoped) {
              out.unindent()
              out("}\n")
            }
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
        outputArgNames(tn, true)
        out(" = ")
        blockBracedToOut(tn.body)
      case tn: AST_Function =>
        outputArgNames(tn)
        out(" => ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
      case tn: AST_Arrow =>
        outputArgNames(tn)
        out(" => ")
        blockBracedToOut(tn.body)
      case tn: AST_Accessor =>
        outputArgNames(tn, true)
        out(" = ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
      case tn: AST_Lambda => outputUnknownNode(tn)
      //case tn: AST_Toplevel => outputUnknownNode(tn)
      //case tn: AST_Scope => outputUnknownNode(tn)
      case tn: AST_DefClass =>
        out.eol(2)
        out"class ${tn.name}"

        // find a constructor and output it
        object NodeIsLambda {
          def unapply(arg: AST_Node) = arg match {
            case l: AST_Lambda => Some(l)
            case _ => None
          }
        }

        val constructor = Transform.findConstructor(tn).flatMap{c => NodeIsLambda.unapply(c.value)}
        val accessor = TransformClasses.classInlineBody(tn)

        outputArgNames(accessor, true)

        for (base <- tn.`extends`) {
          out" extends $base"

          // find the super constructor call and use its parameters
          val superCall = accessor.body.collectFirst {
            case AST_SimpleStatement(call@AST_Call(_: AST_Super, pars@_*)) =>
              out("(")
              outputNodes(pars)(nodeToOut)
              out(")")

          }


        }
        out" {\n"
        out.indent()

        // class body should be a list of variable declarations, constructor statements may follow

        accessor.body.foreach {
          case VarName(s) =>
            val clsName = tn.name.nonNull.map(_.name)
            val sType = input.types.getMember(clsName, s)
            val sTypeName = SymbolTypes.mapSimpleTypeToScala(sType.getOrElse(SymbolTypes.any))
            out"var ${identifier(s)}: $sTypeName\n"
          case AST_SimpleStatement(AST_Call(_: AST_Super, _*)) =>
          case ss =>
            //out(nodeTreeToString(ss))
            nodeToOut(ss)
        }

        //blockToOut(tn.body)

        val (functionMembers, varMembers) = tn.properties.partition {
          case _: AST_ConciseMethod => true
          case _ => false
        }

        //out(s"${functionMembers.length} ${varMembers.length}")
        for (p <- varMembers) {
          nodeToOut(p)
        }
        if ((varMembers.nonEmpty || tn.body.nonEmpty) && constructor.nonEmpty) out.eol(2)

        // call the constructor after all variable declarations
        constructor.foreach { lambda =>
          if (lambda.body.nonEmpty) {
            out("constructor")
            outputArgNames(accessor)
            out.eol()
          }
        }

        if ((constructor.nonEmpty || varMembers.nonEmpty) && functionMembers.nonEmpty) out.eol(2)

        for (p <- functionMembers) {
          if (p.value != accessor) {
            nodeToOut(p)
          }
        }
        out.unindent()
        out.eol()
        out("}\n\n")
        // classes have no body
        //blockBracedToOut(tn.body)
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
      case tn =>
        outputUnknownNode(tn)
        out.eol()
    }
  }

  private def identifier(name: String) = {
    if (Keywords(name)) {
      "`" + name + "`"
    } else name
  }

  private def identifierToOut(out: Output, name: String) = {
    out(identifier(name))
  }

  private def blockBracedToOut(body: js.Array[AST_Statement], force: Boolean = false)(implicit outConfig: Config, input: InputContext, out: Output) = {
    if (!js.isUndefined(body)) { // harmony class may have undefined body
      // TODO: single statement without braces
      out("{\n")
      out.indent()
      blockToOut(body)
      out.unindent()
      out.eol()
      out("}")
    } else {
      out("/* body */ undefined")
    }
  }

  private def blockToOut(body: js.Array[AST_Statement])(implicit outConfig: Config, input: InputContext, out: Output): Unit = {
    for ((s, notLast) <- markEnd(body)) {
      nodeToOut(s)
      if (notLast) out.eol()
    }
  }

  def output(ast: Transform.AST_Extended, input: String, outConfig: Config = Config.default): String = {
    val sb = new StringBuilder
    val ret = new NiceOutput {
      def out(x: String) = sb append x
    }
    val inputContext = InputContext(input, ast.types)
    blockToOut(ast.top.body)(outConfig, inputContext, ret)
    sb.result
  }

  def outputNode(ast: AST_Node, input: String = "", outConfig: Config = Config.default): String = {
    val sb = new StringBuilder
    val ret = new NiceOutput {
      def out(x: String) = sb append x
    }
    val inputContext = InputContext(input, SymbolTypes())
    nodeToOut(ast)(outConfig, inputContext, ret)
    sb.result
  }
}