package com.github.opengrabeso

import net.gamatron.esprima._
import esprima._
import NodeExt._

import JsUtils._
//import Classes._

import scala.util.Try

object ScalaOut {
  import Symbols._

  object Config {
    val default = new Config
  }

  type SymbolDef = SymbolTypes.SymbolMapId

  def nodeClassName(node: Node.Node): String = node.getClass.getSimpleName

  class ClassListHarmony

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
        case s: Node.Identifier =>
          //println("symbol")
          identifierToOut(output, s.name)

          if (false) { // output symbol ids and types
            val symId = SymbolTypes.id(s)
            out"/*${symId.fold(-1)(_.sourcePos)}*/"
            out"/*${input.types.get(symId)}*/"
          }
        case n: Node.Node =>
          nodeToOut(n)
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
  private def printlnNode(n: Node.Node)(implicit outConfig: Config, input: InputContext) = {
    println(nodeClassName(n) + ":" + nodeToString(n))
  }


  class OutToString extends Output {
    val b = new StringBuilder
    def out(x: String) = b append x
    // no smart eol handling - string will be processed when doing proper output
    override def eol(num: Int) = out("\n")
    override def apply(x: String) = out(x)
    def result = b.toString
  }
  def nodeToString(n: Node.Node)(implicit outConfig: Config, input: InputContext): String = {
    val b = new OutToString
    nodeToOut(n)(outConfig, input, b)
    b.result
  }

  def dumpComments(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output) = {
    // start is mostly defined, but not for accessor
    for {
      start <- n.start
      // TODO: handle inner / trailing comments better
      c <- n.leadingComments ++ n.innerComments ++ n.trailingComments
    } {
      if (!(input.commentsDumped contains start)) {
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
        input.commentsDumped += start
      }
    }
  }


  def termToOut(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output): Unit = {

    def outNode(n: Node.Node) = nodeToOut(n)(outConfig, input, out)
    def outInParens(n: Node.Node) = {
      out("(")
      outNode(n)
      out(")")
    }

    n match {
      case Node.BinaryExpression(op, _, _) =>
        op match {
          case `instanceof` | `asinstanceof` =>
            outNode(n)
          case _ =>
            outInParens(n)
        }
      case _: Node.IfStatement =>
        outInParens(n)
      case _: Conditional =>
        outInParens(n)
      case _ =>
        outNode(n)
    }
  }

  def nodeToOut(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output): Unit = {

    def source = nodeSource(n, input.input)
    // http://lisperator.net/uglifyjs/ast
    for (s <- n.start) {
      out.submitLocation(s.pos, source.lines.next)
    }

    def outputVarDef(name: Node.Identifier, initInput: Option[Node.Node], sType: Option[SymbolTypes.TypeDesc], types: Boolean) = {
      out"$name"

      // handle a hack: uninitialized variable using Node.EmptyStatement
      val init = if (initInput.exists(_.isInstanceOf[Node.EmptyStatement])) None else initInput

      //out(s"/*outputVarDef ${name.name} type: $sType init: ${init.map(_.toString)}*/")
      if (types || init.isEmpty) {
        for (tp <- sType) {
          if (tp.typeOnInit) out": ${tp.toOut}"
        }
      }

      def trivialInit(i: Node.Node): Option[Node.Node] = {
        i match {
          case a: NodeExt.Array if a.elements.isEmpty => None
          case o: NodeExt.Object if o.properties.isEmpty => None
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

    def outputDefinitions(isVal: Boolean, tn: Node.Definitions, types: Boolean = false) = {
      //out"/*outputDefinitions ${tn.definitions}*/"
      //println("outputDefinitions -")
      def outValVar(isInitialized: Boolean) = {
        out(if (isVal && isInitialized) "val " else "var ")
      }

      tn.definitions.foreach {

        case Node.VarDef(name, Defined(Node.Object(props))) if props.nonEmpty && isVal =>
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
        case v@Node.VarDef(s@Node.Identifier(name, _, Defined(symDef)), Defined(Node.Object(Seq()))) =>
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

        case NodeExt.VarDef(s@Node.Identifier(name), init) =>
          outValVar(Option(init).isDefined)
          //out("/*outputDefinitions 1*/")
          val sType = getSymbolType(sym)
          //out"/*Node.VarDef sym ${SymbolTypes.id(sym)} $sType*/"
          //println(s"Node.VarDef sym ${SymbolTypes.id(sym)} $sType")
          //println(getType(sym))
          outputVarDef(s, Option(init), sType, types)
      }
    }

    def outputNodes[T](ns: Seq[T])(outOne: T => Unit, delimiter: String = ", ") = {
      for ((arg, delim) <- markEnd(ns)) {
        outOne(arg) + ": Any"
        if (delim) out(delimiter)
      }
    }

    def outputArgType(n: Node.SymbolFunarg, init: Option[Node.Node]) = {
      val typeString = Transform.symbolFromPar(n).fold(SymbolTypes.any.toOut)(input.types.getAsScala(_))
      //println(s"Arg type ${SymbolTypes.id(n.thedef.get)} $typeString")
      out": $typeString"
      for (init <- init) {
        out" = $init"
      }
    }

    def outputClassArgNames(tn: Node.Lambda) = {
      out("(")
      outputNodes(tn.argnames) { n =>
        // parSuffix is still used for parameters which are modified
        if (!input.types.getHint(Transform.symbolId(n)).contains(IsConstructorParameter) && !n.name.endsWith(parSuffix)) {
          out("var ")
        }
        out"$n"
        outputArgType(n, n.init.flatMap(_.headOption))
      }
      out(")")
    }

    def outputArgNames(tn: Node.Lambda, types: Boolean = false) = {
      out("(")
      outputNodes(tn.argnames) { n =>
        val (sym, init: Option[Node.Node]) = n.asInstanceOf[Node.Node] match {
          case d: Node.DefaultAssign =>
            d.left.asInstanceOf[Node.SymbolFunarg] -> Some(d.right)
          case _ =>
            n -> n.init.flatMap(_.headOption)
        }
        out"$sym"
        if (types) {
          outputArgType(sym, init)
        } else {
          val sid = n.thedef.flatMap(SymbolTypes.id)
          for (t <- input.types.get(sid)) {
            out": ${t.declType.toOut}"
          }
        }
      }
      out(")")
    }

    def outputCall(tn: Node.Call) = {
      nodeToOut(tn.expression)
      out("(")
      outputNodes(tn.args)(nodeToOut)
      out(")")
    }

    def outputBinaryArgument(arg: Node.Node, outer: String, right: Boolean = false) = {


      // non-associative need to be parenthesed when used on the right side

      arg match {
        case a@Node.BinaryExpression(_, op, _) if OperatorPriorities.useParens(op, outer, right) =>
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

    def outputUnknownNode(tn: Node.Node, statement: Boolean = false) = {
      def shortNodeClassName(n: String) = {
        val prefix = "Node."
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
    def accessorToOut(tn: Node.ObjectSetterOrGetter, postfix: String) = {
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

    object CanYield {
      def unapply(forIn: Node.ForIn): Option[(Node.Call, Node.Node, Node.Node)] = {
        var countPush = 0
        forIn.body.walk {
          case _: Node.IterationStatement =>
            true // do not check inner loops
          case Node.Call(expr Dot "push", arg) =>
            countPush += 1
            false
          case _ =>
            false
        }

        if (countPush > 1) return None

        // detect if last statement in the for body is a push
        // if it is, convert the loop to yield
        var push = Option.empty[(Node.Call, Node.Node, Node.Node)]
        Transform.walkLastNode(forIn.body) {
          case call@Node.Call(expr Dot "push", arg) =>
            push = Some(call, expr, arg)
            false
          case _ =>
            false
        }

        // verify the loop does not contain any construct which would break transformation
        if (push.isDefined) forIn.body.walk {
          case _: Node.IterationStatement =>
            true // ignore break / continue in inner loops
          case _: Node.Break =>
            push = None
            false
          case _: Node.Continue =>
            push = None
            false
          case _ =>
            false
        }
        push
      }
    }

    def outForHeader(forIn: Node.ForIn) = {
      out("for (")
      forIn.name.fold(nodeToOut(forIn.init))(nodeToOut)
      out(" <- ")
      nodeToOut(forIn.`object`)
      out(") ")
    }


    if (false) {
      out"/*${nodeClassName(n)}*/"
    }

    // listed in reverse order, so that most specific classes match first
    //noinspection ScalaUnusedSymbol
    n match {
      case tn: Node.True => out("true")
      case tn: Node.False => out("false")
      //case tn: Node.Boolean => "Node.Boolean"
      case tn: Node.Infinity => out("Double.PositiveInfinity")
      case tn: Node.Hole => out("null")
      case tn: Node.Undefined => out("null")
      case tn: Node.NaN => out("Double.NaN")
      case tn: Node.Null => out("null")
      //case tn: Node.Atom => "Node.Atom"
      case tn: Node.RegExp => out""""${tn.value}".r"""
      case tn: Node.Number =>
        // prefer the same representation as in the original source
        val src = source

        def decodeInt(s: String) = {
          val prefixes = Seq("0x", "0X", "#")
          for {
            p <- prefixes if s startsWith p
            value <- Try(Integer.parseUnsignedInt(s drop p.length, 16)).toOption
          } yield value
        }

        def decodeDouble(s: String) = Try(s.toDouble).toOption

        def isSourceOf(s: String, number: Double) = {
          decodeInt(s).contains(number) || decodeDouble(s).contains(number)
        }

        if (isSourceOf(src, tn.value)) out(src) else out(tn.value.toString)
      case tn: Node.String => out(quote(tn.value))
      case tn: Node.TemplateString =>
        // TODO: handle expression interpolation
        val value = tn.segments.collect {
          case s: Node.TemplateSegment => s.value
        }.mkString
        out(tripleQuote(value))

      //case tn: Node.Constant => "Node.Constant"
      case tn: Node.This => out("this") // TODO: handle differences between Scala and JS this
      case tn: Node.Super => out("super")
      //case tn: Node.LabelRef => out("Node.LabelRef")
      //case tn: Node.Identifier => out("Node.Identifier")
      //case tn: Node.Label => out("Node.Label")
      //case tn: Node.SymbolCatch => out("Node.SymbolCatch")
      //case tn: Node.SymbolLambda => out("Node.SymbolLambda")
      //case tn: Node.SymbolDefun => out("Node.SymbolDefun")
      //case tn: Node.SymbolConst => out("Node.SymbolConst")
      //case tn: Node.SymbolFunarg => out(tn.name)
      //case tn: Node.SymbolVar => out("Node.SymbolVar")
      //case tn: Node.SymbolDeclaration => out(tn.name)
      //case tn: Node.SymbolAccessor => out("Node.SymbolAccessor")
      //case tn: Node.Identifier => identifierToOut(out, tn.name)
      case tn: Node.Identifier =>
        out"$tn"
      //identifierToOut(out, tn.name)
      case tn: Node.ObjectSetter =>
        accessorToOut(tn, "_=")
      case tn: Node.ObjectGetter =>
        accessorToOut(tn, "")
      case tn: Node.ObjectKeyVal =>
        if (keyValIsTemplate(tn)) {
          tn.value match {
            case Node.Sequence(node: Node.ObjectProperty, Node.String(template)) =>

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
      //case tn: Node.ObjectProperty =>
      case tn: Node.ConciseMethod =>
        //out"/*${nodeClassName(n)}*/"
        out.eol()
        out"def ${tn.key}${tn.value}\n"
      case tn: Node.Object =>
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
      case tn: Node.Array =>
        out("Array(")
        outputNodes(tn.elements)(nodeToOut)
        out(")")
      case tn: Node.Conditional =>
        out"if (${tn.condition}) ${tn.consequent} else ${tn.alternative}"
      //case tn: Node.Assign => outputUnknownNode(tn)
      case Node.BinaryExpression(left, op, right) =>
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
      case tn: Node.Unary =>
        tn.operator match {
          case "typeof" =>
            termToOut(tn.expression)
            out(".getClass")
          case _ =>
            tn match {
              case _: Node.UnaryPrefix =>
                (tn.operator, tn.expression) match {
                  case ("delete", sym Node.Sub prop) =>
                    out"$sym -= $prop"
                  case _ =>
                    out(tn.operator)
                    if (tn.operator.last.isLetterOrDigit) out(" ")
                    termToOut(tn.expression)
                }
              case _: Node.UnaryPostfix =>
                termToOut(tn.expression)
                if (tn.operator.head.isLetterOrDigit) out(" ")
                out(tn.operator)
            }
        }
      case tn: Node.Sub =>
        termToOut(tn.expression)
        out("(")
        nodeToOut(tn.property)
        out(")")
      case tn: Dot =>
        termToOut(tn.expression)
        out(".")
        identifierToOut(out, tn.property)
      //case tn: Node.PropAccess => outputUnknownNode(tn)
      case tn: Node.Sequence =>
        out("{\n")
        out.indent()
        for (item <- tn.expressions) {
          nodeToOut(item)
          out.eol()
        }
        out.unindent()
        out("}")
      case tn: Node.New =>
        out("new ")
        outputCall(tn)
      case tn: Node.Call =>
        outputCall(tn)

      case Node.VarDef(s@Node.Identifier(name, _, Defined(sym)), init) =>
        out("/*Node.VarDef*/")
        val sType = getSymbolType(sym)
        outputVarDef(s, init, sType, false)

      case tn: Node.Const =>
        outputDefinitions(true, tn)
      case tn: Node.Var =>
        outputDefinitions(false, tn) // we assume scoping is reasonable, we do not try to handle hoisting
      case tn: Node.Let =>
        outputDefinitions(false, tn)
      //case tn: Node.Definitions => outputUnknownNode(tn)
      case tn: Node.Continue => outputUnknownNode(tn, true)
      case tn: Node.Break => outputUnknownNode(tn, true)
      //case tn: Node.LoopControl => outputUnknownNode(tn)
      case tn: Node.Throw =>
        out("throw")
        tn.value.foreach { v =>
          out(" ")
          nodeToOut(v)
          out.eol()
        }
      case tn: Node.Return =>
        out("return")
        tn.value.foreach { v =>
          out(" ")
          nodeToOut(v)
          out.eol()
        }
      //case tn: Node.Exit => outputUnknownNode(tn)
      //case tn: Node.Jump => outputUnknownNode(tn)
      case tn: Node.IfStatement =>
        out"if (${tn.condition}) "
        nodeToOut(tn.body)
        tn.alternative.foreach { a =>
          out(" else ")
          nodeToOut(a)
        }
        out.eol()
      case tn: Node.With => outputUnknownNode(tn, true)
      case tn: Node.ForIn =>

        val push = CanYield.unapply(tn)

        push.fold {
          outForHeader(tn)
          nodeToOut(tn.body)
        }{ case (call, expr, arg) =>
          // transform for (a <- array) b.push(f(a)) into b ++= for (a <- array) yield f(a)
          nodeToOut(expr)
          out" ++= "

          out"${tn.`object`}.map { ${tn.name.getOrElse(tn.init)} =>\n"
          out.indent()

          val yieldBody = tn.body.transformBefore {(node, descend, walker) =>
            node match {
              case `call` =>
                arg.clone()
              case _ =>
                val c = node.clone()
                descend(c, walker)
                c
            }
          }
          def bodyFromStatement(s: Node.Statement): Seq[Node.Statement] = {
            s match {
              case b: Node.BlockStatement =>
                b.body
              case _ =>
                Seq(s)
            }
          }
          blockToOut(bodyFromStatement(yieldBody))
          out.unindent()
          out("}")
        }
      case tn: Node.For =>
        tn match {
          case ForRange(name, until, init, end, step) =>
            (init, until, end, step) match {
              case (Node.Number(0), "until", expr Dot "length", Node.Number(1)) =>
                out"for (${name.name} <- $expr.indices) ${tn.body}"

              case _ =>
                out"for (${name.name} <- $init $until $end"
                step match {
                  case Node.Number(1) =>
                  case _ => out" by $step"
                }
                out") ${tn.body}"
            }

          case _ => // generic solution using while - reliable, but ugly
            // new scope never needed in classical JS, all variables exists on a function scope
            val isScoped = tn.init match {
              case Some(Node.Let(_*)) => true
              case _ => false
            }
            if (isScoped) {
              out.eol()
              out("{\n")
              out.indent()
            }
            //out("\n\n{\n")
            tn.init.foreach { init =>
              nodeToOut(init)
            }
            out.eol()
            out("while (")
            tn.condition.fold(out("true"))(nodeToOut)
            out(") {\n")
            out.indent()
            nodeToOut(tn.body)
            out.eol()
            tn.step.foreach(nodeToOut)
            out.unindent()
            out.eol()
            out("}\n")
            if (isScoped) {
              out.unindent()
              out("}\n")
            }
            //out("}\n")
        }

      case tn: Node.While =>
        out("while (")
        nodeToOut(tn.condition)
        out(") ")
        nodeToOut(tn.body)
      case tn: Node.Do =>
        out("do ")
        nodeToOut(tn.body)
        out(" while (")
        nodeToOut(tn.condition)
        out(")\n")
      //case tn: Node.DWLoop => outputUnknownNode(tn)
      //case tn: Node.IterationStatement => outputUnknownNode(tn)
      case tn: Node.LabeledStatement =>
        out(s"/* label ${nodeToString(tn.label)} */")
        nodeToOut(tn.body)
      //case tn: Node.StatementWithBody => outputUnknownNode(tn)
      case tn: Node.EmptyStatement =>
      case tn: Node.Finally =>
        out(" finally ")
        blockBracedToOut(tn.body)
      case tn: Node.Catch =>
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
      case tn: Node.Try =>
        out("try ")
        blockBracedToOut(tn.body)
        tn.bcatch.foreach(nodeToOut)
        tn.bfinally.foreach(nodeToOut)
      case tn: Node.Case =>
        out("case ")

        import Casting._
        object AsInstanceOfCondition extends InstanceOfCondition(asinstanceof)

        def outputCaseBody(body: Seq[Node.Statement]) = {
          out(" =>\n")
          out.indent()
          blockToOut(body)
          out.eol()
          out.unindent()
        }

        tn.expression match {
          // CASE_CAST
          case Node.Call(Node.Identifier("cast_^"),AsInstanceOfCondition(name, classes)) =>
            classes match {
              case Seq(cls) =>
                out"${identifier(name.name + castSuffix)}: $cls"
              case _ =>
                val matchClasses = classes.map(c => s"_: ${identifier(c.name)}").mkString(" | ")
                out(matchClasses)
            }

            tn.body.toSeq match {
              case Seq(Node.BlockStatement(Node.Definitions(Node.VarDef(sv, AsInstanceOfCondition(_, _))) +: body)) =>
                // we might check sv - variable name correspondence
                outputCaseBody(body)
              case _ =>
                outputCaseBody(tn.body)
            }
          case _ =>
            nodeToOut(tn.expression)
            outputCaseBody(tn.body)
        }

      case tn: Node.Default =>
        out("case _ =>\n")
        out.indent()
        blockToOut(tn.body)
        out.eol()
        out.unindent()
      //case tn: Node.SwitchBranch => outputUnknownNode(tn)
      case tn: Node.Switch =>
        nodeToOut(tn.expression)
        out(" match ")
        blockBracedToOut(tn.body, true)
      case tn: Node.Defun =>
        out.eol(2)
        out("def ")
        tn.name.foreach(n => nodeToOut(n))
        outputArgNames(tn, true)
        out(" = ")
        blockBracedToOut(tn.body)
        out.eol()
      case tn: Node.Accessor =>
        outputArgNames(tn, true)
        out(" = ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
        out.eol()
      case tn: Node.Function =>
        outputArgNames(tn)
        out(" => ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
      case tn: Node.Arrow =>
        outputArgNames(tn)
        out(" => ")
        blockBracedToOut(tn.body)
        out.eol()
      case tn: Node.Lambda => outputUnknownNode(tn)
      //case tn: Node.Program => outputUnknownNode(tn)
      //case tn: Node.Scope => outputUnknownNode(tn)
      case tn: Node.DefClass =>

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
          case Defined(Node.SymbolName(`staticClassName`)) =>
            true
          case _ =>
            false
        }

        if (!isStaticOnly) {
          out.eol(2)

          out"class ${tn.name}"

          val inlineBodyOpt = Classes.findInlineBody(tn)

          val constructor = Classes.findConstructor(tn).map(_.value)

          for (inlineBody <- inlineBodyOpt) {
            outputClassArgNames(inlineBody.value)
          }

          for (base <- tn.`extends`) {
            out" extends $base"

            for (inlineBody <- inlineBodyOpt) {
              // find the super constructor call and use its parameters
              inlineBody.value.body.foreach {
                case Node.SimpleStatement(call@Node.Call(_: Node.Super, pars@_*)) =>
                  out("(")
                  outputNodes(pars)(nodeToOut)
                  out(")")
                case _ =>
              }
            }
          }
          out" {\n"
          out.indent()

          for (inlineBody <- inlineBodyOpt) {
            //out"/* inlineBody count ${inlineBody.value.body.length} */\n"
            //out"/* inlineBody ${inlineBody.value.body.mkString(",")} */\n"
            if (false) {
              out"/* inlineBody defs ${
                inlineBody.value.body.collect {
                  case Node.Definitions(Node.VarDef(Node.SymbolName(vn), _)) =>
                    vn
                }.mkString(",")
              } */\n"
            }

            // class body should be a list of variable declarations, constructor statements may follow
            inlineBody.value.body.foreach {
              case df: Node.Const =>
                outputDefinitions(true, df, true)
              case df: Node.Definitions =>
                outputDefinitions(false, df, true)
              case Node.SimpleStatement(Node.Call(_: Node.Super, _*)) =>
              case ss =>
                //out(nodeTreeToString(ss))
                nodeToOut(ss)
            }
          }

          //blockToOut(tn.body)

          val (functionMembers, varMembers) = nonStaticProperties.partition {
            case _: Node.ConciseMethod => true
            case kv: Node.ObjectKeyVal if keyValIsTemplate(kv) => true
            case _ => false
          }


          //out(s"/*fun: ${functionMembers.length} var: ${varMembers.length}*/")
          varMembers.foreach { n =>
            //out"/*${nodeClassName(n)}*/"
            nodeToOut(n)
          }

          if ((varMembers.nonEmpty || tn.body.nonEmpty) && constructor.nonEmpty) out.eol(2)

          if ((constructor.nonEmpty || varMembers.nonEmpty) && functionMembers.nonEmpty) out.eol(2)

          for (pm <- functionMembers if !inlineBodyOpt.contains(pm)) {
            pm match {
              case p: Node.ConciseMethod =>
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
                    def getArgTypes(m: Node.ConciseMethod) = {
                      m.value.argnames.flatMap(_.thedef).map(SymbolTypes.id).map(input.types.get)
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
      case tn: Node.Block =>
        blockBracedToOut(tn.body)
      //case tn: Node.BlockStatement =>
      case tn: Node.SimpleStatement =>
        nodeToOut(tn.body)
        out.eol()
      case tn: Node.Directive =>
        if (source != """"use strict";""" && source != "'use strict';") { // east use strict silently
          outputUnknownNode(tn)
          out.eol()
        }
      case tn: Node.Debugger =>
        outputUnknownNode(tn)
        out.eol()
      case ex: Node.Export if ex.module_name.isEmpty && ex.exported_definition.nonEmpty =>
        out("/* export */ ")
        ex.exported_definition.foreach(nodeToOut)
      case ex: Node.Export if ex.module_name.isEmpty && ex.exported_definition.isEmpty && ex.exported_value.nonEmpty =>
        out("/* export default: */\n")
        ex.exported_value.foreach(nodeToOut)
      case tn: Node.Export =>
        //out(s"/* export */ def ${tn.exported_definition} name ${tn.module_name} value ${tn.exported_value}\n")
        out(s"/* $source */")
      case tn: Node.Import =>
        // try to create a package name from the import directive
        // start from the root
        val imported_names = tn.imported_names.toSeq.flatMap(_.map(_.foreign_name.name))
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

  private def blockBracedToOut(body: js.Array[Node.Statement], force: Boolean = false)(implicit outConfig: Config, input: InputContext, out: Output) = {
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

  private def blockToOut(body: Seq[Node.Statement])(implicit outConfig: Config, input: InputContext, out: Output): Unit = {
    for ((s, notLast) <- markEnd(body)) {
      nodeToOut(s)
      if (notLast) out.eol()
    }
  }

  def output(ast: Transform.NodeExtended, input: String, outConfig: Config = Config.default): Seq[String] = {
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

  def nodeSource(n: Node.Node, input: String): String = {
    (for {
      s <- n.start
      e <- n.end
    } yield input.slice(s.pos, e.endpos)).getOrElse("")
  }

  def outputNode(ast: Node.Node, input: String = "", outConfig: Config = Config.default): String = {
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