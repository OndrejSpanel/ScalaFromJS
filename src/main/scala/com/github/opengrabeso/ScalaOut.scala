package com.github.opengrabeso

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._
import Classes._

import scala.scalajs.js

object ScalaOut {
  import Symbols._

  object Config {
    val default = new Config
  }

  // @param unknowns annotate unknown constructs with a comment (source is always passed through)

  case class Config(unknowns: Boolean = true, parts: Seq[Int] = Seq(Int.MaxValue), root: String = "") {

    def pathToPackage(path: String): String = {
      val parentPrefix = "../"
      val currentPrefix = "./"
      if (path.startsWith(parentPrefix)) pathToPackage(path.drop(parentPrefix.length))
      else if (path.startsWith(currentPrefix)) pathToPackage(path.drop(currentPrefix.length))
      else {
        path.replace('/', '.')
      }
    }

    def withRoot(root: String) = copy(root = root)

    def withParts(parts: Seq[Int]) = copy(parts = parts)

    def formatImport(imported_names: Seq[String], module_name: String, source: String) = {
      // Trim leading or trailing characters from a string? - https://stackoverflow.com/a/25691614/16673
      val trimmedSource = source.replaceAll(";+$", "")
      val comment = s"/* $trimmedSource */\n" // we cannot use //, some imports are multiline in the JS source
      // TODO: when importing many members, use wildcard instead
      val gen = if (imported_names.isEmpty) ""
      else {
        val members = if (imported_names.length > 1) {
          s"{${imported_names.mkString(",")}}"
        } else imported_names.mkString

        // last part of the name is not the package name
        val lastPart = module_name.lastIndexOf('/')
        val modulePath = if (lastPart < 0) module_name else module_name.take(lastPart + 1)

        val pathToImport = pathToPackage(modulePath)
        if (pathToImport.nonEmpty) {
          s"import ${pathToPackage(modulePath)}$members"
        } else ""
      }
      comment + gen
    }
  }

  abstract class Output extends ((String) => Unit) {
    def out(x: String): Unit

    def eol(num: Int = 1): Unit = out("\n")

    def changeIndent(ch: Int): Unit = ()

    def indent(): Unit = changeIndent(+1)
    def unindent(): Unit = changeIndent(-1)

    def submitLocation(loc: Int, debug: =>String): Unit = {}
  }

  abstract class NiceOutput extends Output {
    def out(x: String): Unit

    private var eolDone = Int.MaxValue
    private var indentLevel = 0

    protected def currentIndentLevel = indentLevel

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

  case class InputContext(input: String, types: SymbolTypes, classes: ClassListHarmony) {
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

          if (false) { // output symbol ids and types
            s.thedef.nonNull.fold {
              out"/*thedef == null*/"
            } { df =>
              val symId = SymbolTypes.id(df)
              out"/*${symId.fold(-1)(_.sourcePos)}*/"
              out"/*${input.types.get(symId)}*/"
            }
          }
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

  // extractor for special cases of the for loop
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

      def createRange(vName: String, vValue: AST_Node, rel: String, cRight: AST_Node, assign: String, step: AST_Node) = {
        (rel, assign) match {
          case ("<", "+=") =>
            Some((vName, "until", vValue, cRight, step))
          case ("<=", "+=") =>
            Some((vName, "to", vValue, cRight, step))
          case (">", "-=") =>
            Some((vName, "until", vValue, cRight, negateStep(step)))
          case (">=", "-=") =>
            Some((vName, "to", vValue, cRight, negateStep(step)))
          case _ =>
            None
        }
      }

      (arg.init.nonNull, arg.condition.nonNull, arg.step.nonNull) match {
        // for ( var i = 0; i < xxxx; i += step )
        case (
          Some(VarOrLet(AST_Definitions(AST_VarDef(AST_SymbolName(vName), Defined(vValue))))),
          Some(AST_Binary(AST_SymbolRefName(cLeftName), rel, cRight)),
          Some(AST_Binary(AST_SymbolRefName(exprName), assign, step))
        ) if cLeftName == vName && exprName == vName =>
          createRange(vName, vValue, rel, cRight, assign, step)
        // for ( var i = 0, limit = xxxx; i < limit; i += step )
        case (
          Some(VarOrLet(AST_Definitions(AST_VarDef(AST_SymbolName(vName), Defined(vValue)), AST_VarDef(AST_SymbolName(limitName), Defined(limitValue))))),
          Some(AST_Binary(AST_SymbolRefName(cLeftName), rel, AST_SymbolRefName(cRightName))),
          Some(AST_Binary(AST_SymbolRefName(exprName), assign, step))
        ) if cRightName == limitName && cLeftName == vName && exprName == vName =>
          createRange(vName, vValue, rel, limitValue, assign, step)

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
          // process line by line, fix indenting
          val content = c.value.toString
          out("/*")
          for (l <- content.linesWithSeparators) {
            // note: it might be smarter to check previous indenting level by scanning all lines rather than trimming all whitespaces
            // this is better for ASCI art or tables, where leading white space are used to position the text
            out(l.dropWhile(" \t" contains _))
          }
          out("*/\n")
        } else {
          out"//${c.value}\n"
        }
        input.commentsDumped += c.pos
      }
    }
  }


  def termToOut(n: AST_Node)(implicit outConfig: Config, input: InputContext, out: Output): Unit = {

    def outNode(n: AST_Node) = nodeToOut(n)(outConfig, input, out)
    def outInParens(n: AST_Node) = {
      out("(")
      outNode(n)
      out(")")
    }

    n match {
      case AST_Binary(_, op, _) =>
        op match {
          case `instanceof` | `asinstanceof` =>
            outNode(n)
          case _ =>
            outInParens(n)
        }
      case _: AST_Conditional =>
        outInParens(n)
      case _ =>
        outNode(n)
    }
  }

  def nodeToOut(n: AST_Node)(implicit outConfig: Config, input: InputContext, out: Output): Unit = {

    def source = nodeSource(n, input.input)
    // http://lisperator.net/uglifyjs/ast
    for (s <- n.start) {
      out.submitLocation(s.pos, source.lines.next)
    }

    def outputVarDef(name: AST_Symbol, initInput: js.UndefOr[AST_Node], sType: Option[SymbolTypes.TypeDesc], types: Boolean) = {
      out"$name"

      // handle a hack: uninitialized variable using AST_EmptyStatement
      val init = if (initInput.nonNull.exists(_.isInstanceOf[AST_EmptyStatement])) None else initInput.nonNull

      //out(s"/*outputVarDef ${name.name} type: $sType init: ${init.map(_.toString)}*/")
      if (types || init.isEmpty) {
        for (tp <- sType) {
          if (tp.typeOnInit) out": ${tp.toOut}"
        }
      }

      def trivialInit(i: AST_Node): Option[AST_Node] = {
        i match {
          case a: AST_Array if a.elements.isEmpty => None
          case o: AST_Object if o.properties.isEmpty => None
          case _ => Some(i)
        }
      }

      init.flatMap(trivialInit).fold {
        //println(s"trivialInit of $sType")
        val construct = sType.map(_.scalaConstruct).getOrElse("_")
        out" = $construct"
      }(i => out" = $i")
      out.eol()
    }

    def getSymbolType(symDef: SymbolDef): Option[SymbolTypes.TypeDesc] = {
      //println(s"getSymbolType ${input.types.types}")
      input.types.get(symDef).map(_.declType)
    }

    def outputDefinitions(isVal: Boolean, tn: AST_Definitions, types: Boolean = false) = {
      //out"/*outputDefinitions ${tn.definitions}*/"
      //println("outputDefinitions -")
      def outValVar(isInitialized: Boolean) = {
        out(if (isVal && isInitialized) "val " else "var ")
      }

      tn.definitions.foreach {

        case AST_VarDef(name, Defined(AST_Object(props))) if props.nonEmpty && isVal =>
          // special case handling for isResource marked object (see readFileAsJs)
          val propNames = props.map(propertyName)
          //println(s"propNames $propNames")
          val markerKey = "isResource"
          if ((propNames diff Seq("value", markerKey)).isEmpty) {
            out"object $name extends Resource {\n"
            out.indent()
            for (elem <- props if propertyName(elem)!= markerKey) nodeToOut(elem)
            out.unindent()
            out("}\n")
          } else {
            out"object $name {\n"
            out.indent()
            for (elem <- props) nodeToOut(elem)
            out.unindent()
            out("}\n")
          }
        // empty object - might be a map instead
        case v@AST_VarDef(s@AST_Symbol(name, _, Defined(symDef)), Defined(AST_Object(Seq()))) =>
          val symId = SymbolTypes.id(symDef)
          val tpe = input.types.get(symId).map(_.declType)
          //println(s"Var $name ($symId) type $tpe empty object")
          tpe match {
            case Some(mType: SymbolTypes.MapType) =>
              outValVar(true)
              out"$s = ${mType.scalaConstruct}"
              out.eol()
            case _ =>
              outValVar(false)
              // it has no sense for uninitialized variable to be "val", fix it
              // such variables can be created by extracting class private variables when their initialization cannot be extracted
              outputVarDef(s, js.undefined, tpe, false)
          }

        case AST_VarDef(s@AST_Symbol(name, _, Defined(sym)), init) =>
          outValVar(init.isDefined)
          //out("/*outputDefinitions 1*/")
          val sType = getSymbolType(sym)
          //out"/*AST_VarDef sym ${SymbolTypes.id(sym)} $sType*/"
          //println(s"AST_VarDef sym ${SymbolTypes.id(sym)} $sType")
          //println(getType(sym))
          outputVarDef(s, init, sType, types)
      }
    }

    def outputNodes[T](ns: Seq[T])(outOne: T => Unit, delimiter: String = ", ") = {
      for ((arg, delim) <- markEnd(ns)) {
        outOne(arg) + ": Any"
        if (delim) out(delimiter)
      }
    }

    def outputArgType(n: AST_SymbolFunarg) = {
      val typeString = n.thedef.nonNull.fold(SymbolTypes.any.toOut)(input.types.getAsScala(_))
      //println(s"Arg type ${SymbolTypes.id(n.thedef.get)} $typeString")
      out": $typeString"
      for (init <- n.init.nonNull.flatMap(_.headOption)) {
        out" = $init"
      }
    }

    def outputClassArgNames(tn: AST_Lambda) = {
      out("(")
      outputNodes(tn.argnames) { n =>
        // parSuffix is still used for parameters which are modified
        if (!input.types.getHint(Transform.symbolId(n)).contains(IsConstructorParameter) && !n.name.endsWith(parSuffix)) {
          out("var ")
        }
        out"$n"
        outputArgType(n)
      }
      out(")")
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
            out": ${t.declType.toOut}"
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

    def outputBinaryArgument(arg: AST_Node, outer: String, right: Boolean = false) = {


      // non-associative need to be parenthesed when used on the right side

      arg match {
        case a@AST_Binary(_, op, _) if OperatorPriorities.useParens(op, outer, right) =>
          // TODO: compare priorities
          out"($arg)"
        case _ =>
          out"$arg"
      }

    }


    def quote (s: String): String = "\"" + escape(s) + "\""
    def escape(s: String): String = s.flatMap(escapedChar)
    def tripleQuote(str: String): String = {
      // TODO: handle existing triple quotes
      val triple = "\"\"\""
      triple + str + triple
    }

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
    def accessorToOut(tn: AST_ObjectSetterOrGetter, postfix: String) = {
      out.eol()
      // getter argument list should be empty, setter not
      if (tn.value.argnames.isEmpty) {
        out"def ${tn.key}$postfix = "
        blockBracedToOut(tn.value.body)
        out.eol()
      } else {
        out"def ${tn.key}$postfix"
        outputArgNames(tn.value, true)
        out" = "
        blockBracedToOut(tn.value.body)
        out.eol()
      }
    }

    if (false) {
      out"/*${nodeClassName(n)}*/"
    }

    // listed in reverse order, so that most specific classes match first
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
      case tn: AST_Number =>
        // prefer the same representation as in the original source
        val src = source
        def decodeInt(s: String) = {
          val prefixes = Seq("0x", "0X", "#")
          for (p <- prefixes if s startsWith p) yield Integer.parseUnsignedInt(s drop p.length, 16)
        }
        def isSourceOf(s: String, number: Double) = {
          decodeInt(s).contains(number) || src.toDouble == number
        }
        if (isSourceOf(src, tn.value)) out(src) else out(tn.value.toString)
      case tn: AST_String => out(quote(tn.value))
      case tn: AST_TemplateString =>
        // TODO: handle expression interpolation
        val value = tn.segments.collect {
          case s: AST_TemplateSegment => s.value
        }.mkString
        out(tripleQuote(value))

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
        out"$tn"
        //identifierToOut(out, tn.name)
      case tn: AST_ObjectSetter =>
        accessorToOut(tn, "_=")
      case tn: AST_ObjectGetter =>
        accessorToOut(tn, "")
      case tn: AST_ObjectKeyVal =>
        if (keyValIsTemplate(tn)) {
          tn.value match {
            case AST_Sequence(node: AST_ObjectProperty, AST_String(template)) =>

              val thisPattern = """(\$this|\${this})""".r.unanchored
              val split = thisPattern.pattern.split(template, -1) // -1: do not discard trailing empty match

              if (split.nonEmpty) {
                // if reference to this is present, insert it where necessary
                out.eol()
                out(split.head)
                for (s <- split.tail) {
                  nodeToOut(node)
                  out(s)
                }
                out.eol()
              } else {
                // if not, just output the template
                out(template)
              }
            case x =>
          }
        } else {
          out.eol()
          out"var ${identifier(tn.key)} = ${tn.value}\n"
        }

        //out"/*${nodeClassName(n)}*/"
      //case tn: AST_ObjectProperty =>
      case tn: AST_ConciseMethod =>
        //out"/*${nodeClassName(n)}*/"
        out.eol()
        out"def ${tn.key}${tn.value}\n"
      case tn: AST_Object =>
        if (tn.properties.isEmpty) {
          out("new {}")
        } else {
          out("new {\n") // prefer anonymous class over js.Dynamic.literal
          out.indent()
          tn.properties.foreach { n =>
            nodeToOut(n)
            out.eol()
          }
          out.unindent()
          out.eol()
          out("}")
        }
      case tn: AST_Array =>
        out("Array(")
        outputNodes(tn.elements)(nodeToOut)
        out(")")
      case tn: AST_Conditional =>
        out"if (${tn.condition}) ${tn.consequent} else ${tn.alternative}"
      //case tn: AST_Assign => outputUnknownNode(tn)
      case AST_Binary(left, op, right) =>
        op match {
          case `instanceof` =>
            termToOut(left)
            out".isInstanceOf[$right]"
          case `asinstanceof` =>
            termToOut(left)
            out".asInstanceOf[$right]"
          case _ =>
            outputBinaryArgument(left, op)
            out" $op "
            outputBinaryArgument(right, op, true)
        }
      case tn: AST_Unary =>
        tn.operator match {
          case "typeof" =>
            termToOut(tn.expression)
            out(".getClass")
          case _ =>
            tn match {
              case _: AST_UnaryPrefix =>
                (tn.operator, tn.expression) match {
                  case ("delete", sym AST_Sub prop) =>
                    out"$sym -= $prop"
                  case _ =>
                    out(tn.operator)
                    if (tn.operator.last.isLetterOrDigit) out(" ")
                    termToOut(tn.expression)
                }
              case _: AST_UnaryPostfix =>
                termToOut(tn.expression)
                if (tn.operator.head.isLetterOrDigit) out(" ")
                out(tn.operator)
            }
        }
      case tn: AST_Sub =>
        termToOut(tn.expression)
        out("(")
        nodeToOut(tn.property)
        out(")")
      case tn: AST_Dot =>
        termToOut(tn.expression)
        out(".")
        identifierToOut(out, tn.property)
      //case tn: AST_PropAccess => outputUnknownNode(tn)
      case tn: AST_Sequence =>
        out("{\n")
        out.indent()
        for (item <- tn.expressions) {
          nodeToOut(item)
          out.eol()
        }
        out.unindent()
        out("}")
      case tn: AST_New =>
        out("new ")
        outputCall(tn)
      case tn: AST_Call =>
        outputCall(tn)

      case AST_VarDef(s@AST_Symbol(name, _, Defined(sym)), init) =>
        out("/*AST_VarDef*/")
        val sType = getSymbolType(sym)
        outputVarDef(s, init, sType, false)

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
        out"if (${tn.condition}) "
        nodeToOut(tn.body)
        tn.alternative.nonNull.foreach { a =>
          out(" else ")
          nodeToOut(a)
        }
        out.eol()
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

        import Casting._
        object AsInstanceOfCondition extends InstanceOfCondition(asinstanceof)

        def outputCaseBody(body: Seq[AST_Statement]) = {
          out(" =>\n")
          out.indent()
          blockToOut(body)
          out.eol()
          out.unindent()
        }

        tn.expression match {
          // CASE_CAST
          case AST_Call(AST_SymbolRefName("cast_^"),AsInstanceOfCondition(name, classes)) =>
            classes match {
              case Seq(cls) =>
                out"${identifier(name.name + castSuffix)}: $cls"
              case _ =>
                val matchClasses = classes.map(c => s"_: ${identifier(c.name)}").mkString(" | ")
                out(matchClasses)
            }

            tn.body.toSeq match {
              case Seq(AST_BlockStatement(AST_Definitions(AST_VarDef(sv, AsInstanceOfCondition(_, _))) +: body)) =>
                // we might check sv - variable name correspondence
                outputCaseBody(body)
              case _ =>
                outputCaseBody(tn.body)
            }
          case _ =>
            nodeToOut(tn.expression)
            outputCaseBody(tn.body)
        }

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
        out.eol()
      case tn: AST_Accessor =>
        outputArgNames(tn, true)
        out(" = ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
        out.eol()
      case tn: AST_Function =>
        outputArgNames(tn)
        out(" => ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
      case tn: AST_Arrow =>
        outputArgNames(tn)
        out(" => ")
        blockBracedToOut(tn.body)
        out.eol()
      case tn: AST_Lambda => outputUnknownNode(tn)
      //case tn: AST_Toplevel => outputUnknownNode(tn)
      //case tn: AST_Scope => outputUnknownNode(tn)
      case tn: AST_DefClass =>

        val (staticProperties, nonStaticProperties) = tn.properties.partition(propertyIsStatic)

        if (staticProperties.nonEmpty || nonStaticProperties.isEmpty) {
          out.eol(2)

          out"object ${tn.name} {\n"
          out.indent()
          staticProperties.foreach(nodeToOut)
          out.unindent()
          out("}\n")

        }

        // find a constructor and output it

        val isStaticOnly = tn.`extends` match {
          case Defined(AST_SymbolName(`staticClassName`)) =>
            true
          case _ =>
            false
        }

        for {
          inlineBody <- Classes.findInlineBody(tn)
          if !isStaticOnly
        } {
          out.eol(2)

          out"class ${tn.name}"

          val constructor = Classes.findConstructor(tn).map(_.value)

          outputClassArgNames(inlineBody.value)

          for (base <- tn.`extends`) {
            out" extends $base"

            // find the super constructor call and use its parameters
            inlineBody.value.body.foreach {
              case AST_SimpleStatement(call@AST_Call(_: AST_Super, pars@_*)) =>
                out("(")
                outputNodes(pars)(nodeToOut)
                out(")")
              case _ =>
            }
          }
          out" {\n"
          out.indent()

          //out"/* inlineBody count ${inlineBody.value.body.length} */\n"
          //out"/* inlineBody ${inlineBody.value.body.mkString(",")} */\n"
          if (false) {
            out"/* inlineBody defs ${
              inlineBody.value.body.collect {
                case AST_Definitions(AST_VarDef(AST_SymbolName(vn), _)) =>
                  vn
              }.mkString(",")
            } */\n"
          }

          // class body should be a list of variable declarations, constructor statements may follow
          inlineBody.value.body.foreach {
            case df: AST_Const =>
              outputDefinitions(true, df, true)
            case df: AST_Definitions =>
              outputDefinitions(false, df, true)
            case AST_SimpleStatement(AST_Call(_: AST_Super, _*)) =>
            case ss =>
              //out(nodeTreeToString(ss))
              nodeToOut(ss)
          }

          //blockToOut(tn.body)

          val (functionMembers, varMembers) = nonStaticProperties.partition {
            case _: AST_ConciseMethod => true
            case kv: AST_ObjectKeyVal if keyValIsTemplate(kv) => true
            case _ => false
          }


          //out(s"/*fun: ${functionMembers.length} var: ${varMembers.length}*/")
          varMembers.foreach { n =>
            //out"/*${nodeClassName(n)}*/"
            nodeToOut(n)
          }

          if ((varMembers.nonEmpty || tn.body.nonEmpty) && constructor.nonEmpty) out.eol(2)

          if ((constructor.nonEmpty || varMembers.nonEmpty) && functionMembers.nonEmpty) out.eol(2)

          for (pm <- functionMembers if pm != inlineBody) {
            pm match {
              case p: AST_ConciseMethod =>
                // check overrides
                def isObjectOverride = {
                  // special case: override AnyRef (java.lang.Object) methods:
                  val objectMethods = Set("clone", "toString", "hashCode", "getClass")
                  p.value.argnames.isEmpty && objectMethods.contains(p.key.name)
                }

                def isNormalOverride = {
                  val isOverride = for {
                    parentSym <- Classes.superClass(tn)
                    parentCls <- input.classes.get(parentSym)
                    parentMethod <- Classes.findMethod(parentCls, p.key.name)
                  } yield {
                    // check method signature
                    def getArgTypes(m: AST_ConciseMethod) = {
                      m.value.argnames.flatMap(_.thedef.nonNull).map(SymbolTypes.id).map(input.types.get)
                    }

                    val myParTypes = getArgTypes(p)
                    val parentTypes = getArgTypes(parentMethod)
                    myParTypes.toSeq == parentTypes.toSeq
                  }
                  isOverride.contains(true)
                }

                if (isObjectOverride || isNormalOverride) out("override ")
                out"def ${p.key}${p.value}\n"
              case _ =>
                nodeToOut(pm)
            }
          }

          out.unindent()
          out.eol()
          out("}\n\n")
          // classes have no body
          //blockBracedToOut(tn.body)
        }
      case tn: AST_Block =>
        blockBracedToOut(tn.body)
      //case tn: AST_BlockStatement =>
      case tn: AST_SimpleStatement =>
        nodeToOut(tn.body)
        out.eol()
      case tn: AST_Directive =>
        if (source != """"use strict";""" && source != "'use strict';") { // east use strict silently
          outputUnknownNode(tn)
          out.eol()
        }
      case tn: AST_Debugger =>
        outputUnknownNode(tn)
        out.eol()
      case ex: AST_Export if ex.module_name.isEmpty && ex.exported_definition.nonEmpty =>
        out("/* export */ ")
        ex.exported_definition.foreach(nodeToOut)
      case tn: AST_Export =>
        out(s"/* $source */")
      case tn: AST_Import =>
        // try to create a package name from the import directive
        // start from the root
        val imported_names = tn.imported_names.nonNull.toSeq.flatMap(_.map(_.foreign_name.name))
        val module_name = tn.module_name.value
        val toOut = outConfig.formatImport(imported_names, module_name, source)
        out(toOut)
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

  private def blockToOut(body: Seq[AST_Statement])(implicit outConfig: Config, input: InputContext, out: Output): Unit = {
    for ((s, notLast) <- markEnd(body)) {
      nodeToOut(s)
      if (notLast) out.eol()
    }
  }

  def output(ast: Transform.AST_Extended, input: String, outConfig: Config = Config.default): Seq[String] = {
    val sb = Array.fill(outConfig.parts.size max 1)(new StringBuilder)
    var currentSb = 0
    val ret = new NiceOutput {
      override def out(x: String) = {
        if (currentSb < sb.length) {
          sb(currentSb) append x
        }
      }

      override def submitLocation(loc: Int, debug: =>String) = {
        // start new files only when there is no indenting (top-level)
        if (currentIndentLevel == 0) {
          // check if we have crossed a file boundary, start a new output file if needed
          //println(s"loc $loc of ${outConfig.parts}")
          while (currentSb < outConfig.parts.length && loc >= outConfig.parts(currentSb)) {
            currentSb += 1
            //println(s"Advance to $currentSb at $loc - debug $debug")
          }
        }
      }
    }

    val classListHarmony = new ClassListHarmony(ast)
    val inputContext = InputContext(input, ast.types, classListHarmony)
    blockToOut(ast.top.body)(outConfig, inputContext, ret)
    sb.map(_.result)
  }

  def nodeSource(n: AST_Node, input: String): String = {
    (for {
      s <- n.start
      e <- n.end
    } yield input.slice(s.pos, e.endpos)).getOrElse("")
  }

  def outputNode(ast: AST_Node, input: String = "", outConfig: Config = Config.default): String = {
    val sb = new StringBuilder
    val ret = new NiceOutput {
      def out(x: String) = sb append x
    }
    val classListEmpty = ClassListHarmony(Map.empty)
    val inputContext = InputContext(input, SymbolTypes(), classListEmpty)
    nodeToOut(ast)(outConfig, inputContext, ret)
    sb.result
  }
}