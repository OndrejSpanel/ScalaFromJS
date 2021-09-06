package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, SymId, memberFunId}

import scala.util.Try
import scala.collection.Seq

object ScalaOut {
  import Symbols._

  object Config {
    val default = new Config
  }

  type SymbolDef = Option[SymbolTypes.SymbolMapId]

  import symbols.symId
  import symbols.ScopeContext

  def trimSource(s: String): String = s.replaceAll(";+$", "")

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
      val trimmedSource = trimSource(source)
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
          val path = pathToPackage(modulePath)
          val withDot = if (path.endsWith(".")) path else path + "."
          s"import $withDot$members"
        } else ""
      }
      comment + gen
    }
  }

  abstract class Output extends (String => Unit) {
    def out(x: String): Unit

    def appendLine(x: String): Unit = apply(x)

    def eol(num: Int = 1): Unit = out("\n")


    def flush() = {}

    def changeIndent(ch: Int): Unit = ()

    def indent(): Unit = changeIndent(+1)
    def unindent(): Unit = changeIndent(-1)

    def submitLocation(loc: Int, debug: =>String): Unit = {}
  }

  abstract class NiceOutput extends Output {
    def out(x: String): Unit


    private var eolDone = Int.MaxValue
    private var eolPending = 0
    private var indentLevel = 0

    def isIndented: Boolean = indentLevel > 0

    override def changeIndent(ch: Int): Unit = indentLevel += ch

    private def dumpLine(line: String) = {
      out(line)
      if (line == "\n") eolDone += 1
      else eolDone = if (line.lastOption.contains('\n')) 1 else 0
    }
    private def singleLine(line: String) = {
      doEol()
      if (eolDone > 0) out(" " * (indentLevel * 2))
      dumpLine(line)
    }

    override def appendLine(v: String): Unit = {
      val lines = v.linesWithSeparators.toSeq
      for (line <- lines.headOption) {
        // separate what we are appending (a comment) with a space
        dumpLine(" " + line)
      }
      for (line <- lines.drop(1)) {
        singleLine(line)
      }

    }



    private def doEol(): Unit = {
      while (eolDone < eolPending) {
        out("\n")
        eolDone += 1
      }
      eolPending = 0

    }

    override def eol(num: Int) = {
      // num is number of total eols wanted
      // eolDone was already done, eolPending is how many will we need
      val toEol = num - eolDone
      if (toEol > eolPending) eolPending = toEol
    }

    def apply(v: String) = {
      val lines = v.linesWithSeparators
      for (line <- lines) {
        singleLine(line)
      }

    }

    override def flush() = {
      doEol()
    }

  }

  case class InputContext(input: String, types: SymbolTypes, classes: ClassListHarmony) {
    var commentsDumped = Set.empty[IdentityBox[CommentHandler.Comment]]
  }

  object OutStringContext {
    def outIdentifier(s: String, sid: =>SymId)(implicit input: InputContext, output: Output) = {
      identifierToOut(output, s)

      if (false || SymbolTypes.watched(s)) { // output symbol ids and types
        val s = sid
        output.out(s"/*${s.sourcePos}*/")
        output.out(s"/*${input.types.get(s)}*/")
      }

    }

  }
  implicit class OutStringContext(val sc: StringContext)(implicit outConfig: Config, input: InputContext, output: Output, context: ScopeContext) {

    def outEx(ex: Any): Unit = {
      ex match {
        case s: String =>
          //println("symbol")
          output(s)
        case s: Node.Identifier =>
          // caution: using symId does not work for class member call access, as that requires a special symId - memberFunId
          val detectMemberCalls = false
          if (detectMemberCalls) {
            val plainScope = context.findScope(s.name, false)
            val classScope = context.findScope(s.name, true)
            if (plainScope != classScope) {
              // this most often happens when a property is declared in a d.ts interface
              // and used as a plain member later
              // perhaps we could handle such properties in some special way at the declaration?
              println(s"Warning: class scope detected but not used for ${s.name}")
            }
          }
          OutStringContext.outIdentifier(s.name, symId(s.name).get)
        case t: SymbolTypes.TypeDesc =>
          val resolved = input.types.resolveType(t)
          output(resolved.toOut)
        case n: Node.Node =>
          nodeToOut(n)
        case null =>
          output("??? /*null*/")
        case any =>
          output(any.toString)
      }
    }

    def out(args: Any*): Unit = {
      val strings = sc.parts.iterator
      val expressions = args.iterator

      import StringContext.{processEscapes=>escape}
      output(escape(strings.next()))
      while(strings.hasNext) {
        val ex = expressions.next()
        outEx(ex)
        output(escape(strings.next()))
      }
      assert(!strings.hasNext)
      assert(!expressions.hasNext)
    }
  }


  def markEnd[T](seq: Seq[T]) = seq zip (seq.drop(1).map(_ => true) :+ false)


  //noinspection ScalaUnusedSymbol
  private def printlnNode(n: Node.Node)(implicit outConfig: Config, input: InputContext, context: ScopeContext) = {
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
  def nodeToString(n: Node.Node)(implicit outConfig: Config, input: InputContext, context: ScopeContext): String = {
    val b = new OutToString
    nodeToOut(n)(outConfig, input, b, context)
    b.result
  }



  def dumpComments(comments: Seq[CommentHandler.Comment])(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext) = {
    for {
      nonNull <- Option(comments)
      c <- nonNull
    } {
      if (!(input.commentsDumped contains new IdentityBox(c))) {

        def checkBegLine(range: (Int, Int)) = {
          val begLine = input.input.lastIndexOf('\n', c.range._1 - 1) + 1 max 0
          val linePrefix = input.input.slice(begLine, c.range._1)
          linePrefix.forall(_.isWhitespace)
        }

        def checkEndLine(range: (Int, Int)) = {
          val endLine = input.input.indexOf('\n', c.range._2)
          val lineRest = input.input.slice(c.range._2, endLine)
          lineRest.forall(_.isWhitespace)
        }

        out.submitLocation(c.range._1, "comment")
        if (c.`type` == "Block") {
          // process line by line, fix indenting
          val content = c.value.toString
          if (!checkBegLine(c.range)) out(" ")
          out("/*")
          for (l <- content.linesWithSeparators) {
            // note: it might be smarter to check previous indenting level by scanning all lines rather than trimming all whitespaces
            // this is better for ASCI art or tables, where leading white space are used to position the text
            out(l.dropWhile(" \t" contains _))
          }
          out("*/")
          if (checkEndLine(c.range)) {
            out.eol()
          }
        } else {
          // check if comment was started on a new line
          // parser does not tell us this, we need to check input

          // check if there are any characters between line start and this comment prefix
          // comment range starts on the comment prefix // (the value does not include it)

          val str = s"//${c.value}".trim
          if (!checkBegLine(c.range)) {
            out.appendLine(str)
          } else {
            out.eol()
            out(str)
          }
          out.eol()
        }
        input.commentsDumped += new IdentityBox(c)
      }
    }

  }

  def dumpLeadingComments(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext) = {
    dumpComments(n.leadingComments)
  }

  def dumpInnerComments(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext) = {
    dumpComments(n.innerComments)
  }

  def dumpTrailingComments(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext) = {
    dumpComments(n.innerComments) // if somebody did not call innerComments, do it now - calling twice will ignore the second call
    /*
    if (Option(n.trailingComments).exists(_.nonEmpty)) {
      out.eol()
    }
    */
    dumpComments(n.trailingComments)
  }

  def termToOut(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext): Unit = {

    def outNode(n: Node.Node) = nodeToOut(n)(outConfig, input, out, context)
    def outInParens(n: Node.Node) = {
      out("(")
      outNode(n)
      out(")")
    }

    n match {
      case Node.BinaryExpression(op, _, _) =>
        op match {
          case `instanceof` | `asinstanceof` | "as" =>
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

  def nodeToOut(n: Node.Node)(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext): Unit = {

    implicit class OutResolved(t: SymbolTypes.TypeDesc) {
      def toOutResolved: String = input.types.resolveType(t).toOut
    }

    assert(n != null)
    context.withScope(n) {

      def source = nodeSource(n, input.input)
      // http://lisperator.net/uglifyjs/ast
      for (s <- n.start) {
        out.submitLocation(s, source.linesIterator.next())
      }

      def outputVarDef(name: Node.Identifier, initInput: Option[Node.Node], sType: Option[SymbolTypes.TypeDesc], types: Boolean) = {
        out"$name"

        // handle a hack: uninitialized variable using Node.EmptyStatement
        val init = if (initInput.exists(_.isInstanceOf[Node.EmptyStatement])) None else initInput

        //out(s"/*outputVarDef ${name.name} type: $sType init: ${init.map(_.toString)}*/")
        if (types || init.isEmpty) {
          for (tp <- sType) {
            if (tp.typeOnInit) out": $tp"
          }
        }

        def trivialInit(i: Node.Node): Option[Node.Node] = {
          i match {
            case a: AArray if a.elements.isEmpty => None
            case o: OObject if o.properties.isEmpty => None
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

      def getSymbolType(symDef: SymId): Option[(SymbolTypes.TypeDesc, Boolean)] = {
        //println(s"getSymbolType ${input.types.types}")
        input.types.types.get(symDef).map(s => (s.declType, s.certain))
      }

      def outputDefinitions(isVal: Boolean, tn: Node.VariableDeclaration, types: Boolean = false) = {
        //out"/*outputDefinitions ${tn.definitions}*/"
        //println("outputDefinitions -")
        def outValVar(isInitialized: Boolean) = {
          out(if (isVal && isInitialized) "val " else "var ")
        }

        tn.declarations.foreach { node =>
          context.scanSymbols(node)
          node match {

            case Node.VariableDeclarator(name: Node.Identifier, oe@OObject(props), tpe) if props.nonEmpty && isVal =>
              val propNames = props.map(propertyName)
              //println(s"propNames $propNames")
              val markerKey = "isResource"
              context.withScope(oe) {
                if (propNames.toSet == Set("value", markerKey)) { // special case handling for isResource marked object (see readFileAsJs)
                  out"object $name extends Resource {\n"
                  out.indent()
                  for (elem <- props if propertyName(elem) != markerKey) nodeToOut(elem)
                  out.unindent()
                  out("}\n")
                } else if (tpe != null) { // special handling for enums
                  out"object $name"
                  val parent = astType(Option(tpe))
                  parent.foreach(p => out" extends $p")
                  out" {\n"
                  out.indent()
                  for (elem <- props) nodeToOut(elem)
                  out.unindent()
                  out("}\n")
                } else {
                  out"object $name {\n"
                  out.indent()
                  for (elem <- props) nodeToOut(elem)
                  out.unindent()
                  out("}\n")
                }
              }
            case Node.VariableDeclarator(s: Node.Identifier, OObject(Seq()), vType) =>
              // empty object - might be a map instead
              val sid = symId(s)
              val tpe = input.types.get(sid).map(_.declType).orElse(astType(Option(vType)))
              //println(s"Var $name ($sid) type $tpe empty object")
              tpe match {
                case Some(mType: SymbolTypes.MapType) =>
                  outValVar(true)
                  out"$s = ${mType.scalaConstruct}"
                  out.eol()
                case _ =>
                  outValVar(false)
                  // it has no sense for uninitialized variable to be "val", fix it
                  // such variables can be created by extracting class private variables when their initialization cannot be extracted
                  outputVarDef(s, None, tpe, false)
              }
            case VarDef(s@Node.Identifier(Id(name)), init, vType) =>
              outValVar(init != null)
              //out("/*outputDefinitions 1*/")
              val sType = getSymbolType(name).orElse(astType(Option(vType)).map(_ -> true))
              //out"/*Node.VariableDeclarator sym ${SymbolTypes.id(sym)} $sType*/"
              //println(s"Node.VariableDeclarator sym ${SymbolTypes.id(sym)} $sType")
              //println(getType(sym))
              outputVarDef(s, Option(init), sType.map(_._1), types || sType.exists(_._2))
            case VarDef(Node.ArrayPattern(names), init, vType) =>
              out"val (${names.head}"
              for (n <- names.tail) out", $n"
              if (node.init != null) {
                out" = ${node.init}"
              }
            case _ =>
              out"/* Unsupported var */ var ${node.id}"
              if (node.init != null) {
                out" = ${node.init}"
              }
          }
        }
      }

      def outputNodes[T <: Node.Node](ns: Seq[T], root: Option[Node.Node] = None)(outOne: T => Unit, delimiter: String = ", ") = {
        // try to respect line boundaries as specified in the input
        val firstLine = root.orElse(ns.headOption).flatMap(h => Option(h.loc)).map(_.start.line).getOrElse(0)
        val lastLine = root.orElse(ns.headOption).flatMap(h => Option(h.loc)).map(_.end.line).getOrElse(0)
        var currentLine = firstLine
        for ((arg, delim) <- markEnd(ns) if arg != null) {
          val argLine = Option(arg.loc).map(_.start.line).getOrElse(currentLine)
          if (root.isDefined && argLine > currentLine) {
            out.eol()
            currentLine = argLine
          }
          outOne(arg)
          if (delim) {
            out(delimiter)
          }
        }
        if (root.isDefined && lastLine > currentLine) out.eol()
      }

      def astType(vType: Option[Node.TypeAnnotation]): Option[SymbolTypes.TypeDesc] = {
        vType.flatMap(transform.TypesRule.typeFromAST(_)(context))
      }

      def outputArgType(n: Node.Identifier, init: Option[Node.Node], tpe: Option[Node.TypeAnnotation])(scopeNode: Node.Node) = {
        val scope = ScopeContext.getNodeId(scopeNode)
        val sid = SymId(n.name, scope)
        val typeDecl = input.types.get(Some(sid)).map(_.declType).orElse(astType(tpe)).getOrElse(SymbolTypes.AnyType)
        if (SymbolTypes.watched(sid.name)) { // output symbol ids and types
          out"/*$sid*/"
        }
        //println(s"Arg type ${SymbolTypes.id(n.thedef.get)} $typeDecl")
        out": $typeDecl"
        for (init <- init) {
          out" = $init"
        }
      }

      def outputClassArgNames(argnames: Seq[Node.FunctionParameter])(scopeNode: Node.Node) = {
        //noinspection ScalaUnusedSymbol
        val ctx = null // hide implicit context scope
        val scopeId = ScopeContext.getNodeId(scopeNode)
        out("(")
        outputNodes(argnames) { n =>
          dumpLeadingComments(n)
          val (sym, tpe, init) = Transform.funArg(n)
          // parSuffix is still used for parameters which are modified
          if (!input.types.getHint(Some(SymId(sym.name, scopeId))).contains(IsConstructorParameter) && !sym.name.endsWith(parSuffix)) {
            out("var ")
          }
          out(identifier(sym.name))
          outputArgType(sym, init, Option(tpe))(scopeNode)
          dumpTrailingComments(n)
        }
        out(")")
      }

      def outputTypeParameters(typeParameters: Node.TypeParameterList) = {
        if (typeParameters != null) {
          out("[")
          var first = true
          for (t <- typeParameters.types) {
            if (!first) out", "
            first = false
            out"${t.name}"
            if (t.constraint != null) {
              out(" <: ")
              out(transform.TypesRule.typeFromAST(t.constraint)(context).getOrElse(SymbolTypes.AnyType).toOutResolved)
            }
          }
          out("]")
        }
      }
      def outputTypeArguments(typeArgs: Seq[Node.TypeAnnotation]) = {
        if (typeArgs != null) {
          out("[")
          for (t <- typeArgs) {
            val tt = transform.TypesRule.typeFromAST(t)(context).getOrElse(SymbolTypes.AnyType)
            out"$tt"
          }
          out("]")
        }
      }

      def outputArgNames(argnames: Seq[Node.FunctionParameter], types: Boolean = false)(scopeNode: Node.Node) = {
        //noinspection ScalaUnusedSymbol
        val ctx = null // hide implicit context scope
        out("(")
        outputNodes(argnames) { n =>
          dumpLeadingComments(n)
          val fa = Transform.funArg(n)
          //val (sym, MayBeNull(tpe), init) = fa
          // ^^^ 2.12 version no longer works in 2.13, Extractor pattern is null safe
          // see https://scala-lang.org/files/archive/spec/2.13/08-pattern-matching.html
          // and https://github.com/scala/scala/pull/6485
          val sym = fa._1
          val tpe = Option(fa._2)
          val init = fa._3
          out"$sym"
          if (types) {
            outputArgType(sym, init, tpe)(scopeNode)
          } else {
            val scope = ScopeContext.getNodeId(scopeNode)
            val sid = SymId(sym.name, scope)
            for (t <- input.types.get(Some(sid))) {
              out": ${t.declType}"
            }
          }
          dumpTrailingComments(n)
        }
        out(")")
      }

      def outputCall(callee: Node.Node, typeArgs: Seq[Node.TypeAnnotation], arguments: Seq[Node.ArgumentListElement]) = {
        nodeToOut(callee)
        outputTypeArguments(typeArgs)
        out("(")
        outputNodes(arguments)(nodeToOut)
        out(")")
      }

      def outputMethod(key: Node.PropertyKey, value: Node.PropertyValue, kind: String, tpe: Option[Node.TypeAnnotation], decl: String = "def") = {
        val name = propertyKeyName(key)
        val memberSymId = context.findClassScope.map {
          case cls: Node.ClassDeclaration =>
            SymId(name, ScopeContext.getNodeId(cls.body))
          case oe: Node.ObjectExpression =>
            SymId(name, ScopeContext.getNodeId(oe))
        }.getOrElse(SymId.global(name))

        if (kind == "value") {
          val sType = input.types.get(memberSymId)
          out"var $key"
          sType.map(_.declType).orElse(astType(tpe)).foreach(t => out": ${t.toOut}")
          if (value != null) {
            out" = $value"
          }
          out("\n")
        } else value match {
          case f@Node.FunctionExpression(_, params, body, generator, dType) =>
            context.withScope(f) {
              val postfix = if (kind == "set") "_=" else ""
              //val memberFunId(propertyKeyName(key))
              out("def ")
              OutStringContext.outIdentifier(name, memberSymId)
              out(postfix)
              if (kind != "get" || params.nonEmpty) {
                outputArgNames(params, true)(value)
              }
              def certainType(id: SymId) = {
                if (kind == "set") Some(SymbolTypes.NoType) // setter always returns unit
                else {
                  val tpe = input.types.types.get(id)
                  if (tpe.exists(_.certain)) {
                    // unless it is a getter, tpe should be a function type
                    (kind, tpe.map(_.declType)) match {
                      case ("get", t) =>
                        t
                      case (_, Some(SymbolTypes.FunctionType(ret, _))) =>
                        // Any is most often result of "this" type
                        if (ret == SymbolTypes.AnyType) None // rather than outputing Any do not output anything
                        else Some(ret)
                      case (_, t) =>
                        // there is some confusion regarding if member function type is a function type or not
                        // the code above expects a function type, but when a function type was added in handleParameterTypes
                        // type inference was all broken (function types were assumed as a result type whenever the function was used)
                        // to stay on the safe side, when it is not a function type, we just print it
                        t
                    }
                  } else {
                    astType(Option(dType))
                  }
                }
              }
              for {
                fType <- certainType(memberSymId)
              } {
                out": $fType"
              }
              if (body != null) {
                out(" = ")
                //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
                context.withScope(body) {
                  blockBracedToOut(body)
                }
              }
              out("\n") // use this to better match Uglify.js based version output - this is not counted in NiceOutput.eolDone
              //out.eol()
            }

          case _ =>
            key match {
              case LiteralAsName(v) =>
                val keyName = identifier(v)
                // avoid literals used as object keys to be quoted
                out"$decl $keyName = $value\n"
              case _ =>
                out"$decl $key = $value\n"

            }
        }
      }

      def outputObjectLiteral(tn: OObject): Unit = {
        val delimiter = ", "
        // TODO: DRY with outputNodes
        out("Map(")
        // try to respect line boundaries as specified in the input
        val firstLine = Option(tn.loc).map(_.start.line).getOrElse(0)
        val lastLine = Option(tn.loc).map(_.end.line).getOrElse(0)
        var currentLine = firstLine
        out.indent()
        if (lastLine > firstLine) out.eol()
        for ((arg, delim) <- markEnd(tn.properties) if arg != null) {
          val argLine = Option(arg.loc).map(_.start.line).getOrElse(currentLine)
          arg match {
            case Node.SpreadElement(argument) =>
              out"/* spread */ "
              nodeToOut(argument)
            case Node.PropertyEx(kind, key, computed, value, method, shorthand, readonly) =>
              // what about method and other properties?
              val name = propertyKeyName(key)
              out""""$name" -> """
              nodeToOut(value)
          }
          if (delim) {
            out(delimiter)
            if (argLine > currentLine) {
              out.eol()
              currentLine = argLine
            }
          }
        }
        if (lastLine > currentLine) out.eol()
        out.unindent()
        out(")")
      }

      def outputObjectLiteralUsingNew(tn: OObject): Unit = {
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
      }

      def outputMethodNode(pm: Node.ClassBodyElement, decl: String = "def", eol: Boolean = true) = {
        if (eol) out.eol()
        pm match {
          case Node.MethodDefinition(null, tpe, _, null, _, _) =>
            // probably IndexSignature - we cannot output that in Scala
            out"/* IndexSignature ${astType(Option(tpe)).getOrElse(SymbolTypes.AnyType).toOutResolved} */\n"
          case p: Node.MethodDefinition =>
            outputMethod(p.key, p.value, p.kind, Option(p.`type`), decl)
          case _ =>
            nodeToOut(pm)
        }
      }

      def outputBinaryArgument(arg: Node.Node, outer: String, right: Boolean = false) = {
        // non-associative need to be parenthesed when used on the right side
        arg match {
          case Node.BinaryExpression(op, _, _) if OperatorPriorities.useParens(op, outer, right) =>
            // TODO: compare priorities
            out"($arg)"
          case _: Node.IfStatement | _: Conditional =>
            termToOut(arg)
          case _ =>
            out"$arg"
        }

      }


      def quote(s: String): String = "\"" + escape(s) + "\""

      def escape(s: String): String = s.flatMap(escapedChar)

      def tripleQuote(str: String): String = {
        // TODO: handle existing triple quotes
        val triple = "\"\"\""
        triple + str + triple
      }

      // https://stackoverflow.com/a/40073137/16673
      def escapedChar(ch: Char): String = ch match {
        case '\b' => "\\b"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case '"' => "\\\""
        //case '\'' => "\\\'" // no need to escape, will be used in quotes
        case '\\' => "\\\\"
        case _ => if (ch.isControl) "\\u" + f"$ch%04X"
        else String.valueOf(ch)
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

      dumpLeadingComments(n)

      //noinspection ScalaUnusedSymbol
      /*
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
    */

      object CanYield {

        object IsLoop {
          def unapply(node: Node.Node): Boolean = node match {
            case _: Node.ForInStatement => true
            case _: Node.ForOfStatement => true
            case _: Node.ForStatement => true
            case _: Node.DoWhileStatement => true
            case _: Node.WhileStatement => true
            case _ =>
              false
          }
        }

        def unapply(forIn: Node.ForInStatement): Option[(Node.CallExpression, Node.Node, Node.Node)] = {
          var countPush = 0
          forIn.body.walk {
            // do not check inner loops
            case IsLoop() => true
            case Node.CallExpression(expr Dot "push", arg) =>
              countPush += 1
              false
            case _ =>
              false
          }

          if (countPush > 1) return None

          // detect if last statement in the for body is a push
          // if it is, convert the loop to yield
          var push = Option.empty[(Node.CallExpression, Node.Node, Node.Node)]
          Transform.walkLastNode(forIn.body) {
            case call@Node.CallExpression(expr Dot "push", Seq(arg)) =>
              push = Some(call, expr, arg)
              false
            case _ =>
              false
          }

          // verify the loop does not contain any construct which would break transformation
          if (push.isDefined) forIn.body.walk {
            case IsLoop() =>
              true // ignore break / continue in inner loops
            case _: Node.BreakStatement =>
              push = None
              false
            case _: Node.ContinueStatement =>
              push = None
              false
            case _ =>
              false
          }
          push
        }
      }

      def outForHeader(forIn: Node.ForInStatement) = {
        out("for (")
        val variable = forIn.left match {
          case Node.Identifier(name) =>
            name
          case VarDecl(name, _, _) =>
            name
          case Node.EmptyStatement() =>
            "_"
        }
        out(identifier(variable))
        out(" <- ")
        nodeToOut(forIn.right)
        out(") ")
      }


      if (false) {
        out"/*${nodeClassName(n)}*/"
      }

      // listed in reverse order, so that most specific classes match first
      //noinspection ScalaUnusedSymbol
      n match {
        //case tn: Node.Atom => "Node.Atom"
        case tn: Node.RegexLiteral => out""""${tn.raw}".r"""
        case StringLiteral(str) =>
          val escaped = escape(str)
          out(s""""$escaped"""")
        case Node.Literal(null, _) =>
          out("null")
        case Node.Literal(Defined(value), raw) =>
          // prefer the same representation as in the original source
          value.value match {
            case value: Double =>
              // note: raw coming from esprima often contains trailing semicolon, trim it if needed
              // this was probably a bug in the token handling which is already fixed
              val src = if (raw.lastOption.contains(';')) raw.dropRight(1) else raw

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

              if (isSourceOf(src, value)) out(src) else out(value.toString)
            case value: Boolean =>
              out(value.toString)
          }
        case tn: Node.TemplateLiteral =>
          // TODO: handle expression interpolation
          val value = tn.quasis.collect {
            case s: Node.TemplateElement => s.value.raw
          }.mkString
          out(tripleQuote(value))

        case Node.ExportNamedDeclaration(VarDecl(name, Some(Node.Literal(value, raw)), "const"), Seq(), null) if name.startsWith(ConvertProject.prefixName) =>
          // markers are syntetic, we do not want them in the output, unless debugging
          if (false) {
            val content = name.drop(ConvertProject.prefixName.length)
            // we might have create synthetic names to stay unique in each module (see declaredGlobal)
            out(s"// ${content.takeWhile(_ != '$')} - $value")
          }

        case VarDecl(name, Some(Node.Literal(value, raw)), "const") if name.startsWith(ConvertProject.prefixName) =>
          // this should not happen, but when it does, we ignore it

        //case tn: Node.Constant => "Node.Constant"
        case tn: Node.ThisExpression => out("this") // TODO: handle differences between Scala and JS this
        case tn: Node.Super => out("super")
        //case tn: Node.LabelRef => out("Node.LabelRef")
        //case tn: Node.Identifier => out("Node.Identifier")
        //case tn: Node.Label => out("Node.Label")
        //case tn: Node.SymbolCatch => out("Node.SymbolCatch")
        //case tn: Node.SymbolLambda => out("Node.SymbolLambda")
        //case tn: Node.SymbolDefun => out("Node.SymbolDefun")
        //case tn: Node.SymbolConst => out("Node.SymbolConst")
        //case tn: Node.FunctionParameter => out(tn.name)
        //case tn: Node.SymbolVar => out("Node.SymbolVar")
        //case tn: Node.SymbolDeclaration => out(tn.name)
        //case tn: Node.SymbolAccessor => out("Node.SymbolAccessor")
        //case tn: Node.Identifier => identifierToOut(out, tn.name)
        case tn: Node.Identifier =>
          out"$tn"
        case Node.TypeAliasDeclaration(name, tpe) =>
          val tt = astType(Some(tpe)).getOrElse(SymbolTypes.AnyType)
          out"type $name = $tt\n"
        //identifierToOut(out, tn.name)
        case tn@ScalaNode.MemberTemplate(name, original, template) =>

          val thisPattern = """(\$this|\$\{this\})""".r.unanchored
          val split = thisPattern.pattern.split(template, -1) // -1: do not discard trailing empty match

          if (split.nonEmpty) {
            // if reference to this is present, insert it where necessary
            out.eol()
            out(split.head)
            for (s <- split.tail) {
              nodeToOut(original)
              out(s)
            }
            out.eol()
          } else {
            // if not, just output the template
            out(template)
          }
          out("\n")

        case Node.PropertyEx(kind, key, _, value, _, _, readOnly) =>
          out.eol()
          outputMethod(key, value, kind, None,if (readOnly) "val" else "var")

        //out"/*${nodeClassName(n)}*/"
        //case tn: Node.ObjectProperty =>
        case tn: Node.MethodDefinition =>
          //out"/*${nodeClassName(n)}*/"
          out.eol()
          outputMethod(tn.key, tn.value, tn.kind, Option(tn.`type`))


        case tn: OObject =>
          // None means "I do not know", true is use Map, false is use new
          def useMap(node: Node.Node): Option[Boolean] = node match {
            case _: Node.VariableDeclaration =>
              Some(true)
            case _: Node.AssignmentExpression =>
              Some(false)
            case _: Node.CallExpression =>
              Some(false)
            case _: Node.ArrayExpression =>
              Some(false)
            case _: Node.NewExpression =>
              Some(false)
            case _: Node.FunctionDeclaration =>
              Some(false) // esp. intended to catch return values
            case _: Node.FunctionExpression =>
              Some(false) // esp. intended to catch return values
            case _ =>
              None
          }
          context.parents.reverseIterator.flatMap(useMap).nextOption() match {
            case Some(true) =>
              outputObjectLiteral(tn)
            case Some(false) =>
              outputObjectLiteralUsingNew(tn)
            case None => // default
              outputObjectLiteralUsingNew(tn)
          }

        case tn: AArray =>
          out("Array(")
          out.indent()
          outputNodes(tn.elements, Some(tn))(nodeToOut)
          out.unindent()
          out(")")
        case tn: Node.ConditionalExpression =>
          out"if (${tn.test}) ${tn.consequent} else ${tn.alternate}"
        //case tn: Node.Assign => outputUnknownNode(tn)
        case Node.AssignmentExpression(op, left, right) =>
          nodeToOut(left)
          out" $op "
          nodeToOut(right)
        case Node.BinaryExpression(op, left, right) =>
          def typeNameFromExpression(ex: Node.Expression): String = {
            val tpe = ex match {
              case Node.Identifier(rTypeName) =>
                transform.TypesRule.typeFromIdentifierName(rTypeName, false)(context).map(_.toOutResolved)
              case Node.StaticMemberExpression(obj, property) =>
                Some(typeNameFromExpression(obj) + "." + nodeToString(property))
              case _ =>
                None
            }
            tpe.getOrElse(right.toString)
          }
          op match {
            case `instanceof` =>
              termToOut(left)
              out".isInstanceOf[${typeNameFromExpression(right)}]"
            case `asinstanceof` | "as" =>
              termToOut(left)
              out".asInstanceOf[${typeNameFromExpression(right)}]"
            case _ =>
              outputBinaryArgument(left, op)
              out" $op "
              outputBinaryArgument(right, op, true)
          }
        case tn: Node.UnaryExpression =>
          tn.operator match {
            case "typeof" =>
              termToOut(tn.argument)
              out(".getClass")
            case _ =>
              if (tn.prefix) {
                (tn.operator, tn.argument) match {
                  case ("delete", sym Sub prop) =>
                    out"$sym -= $prop"
                  case _ =>
                    out(tn.operator)
                    if (tn.operator.last.isLetterOrDigit) out(" ")
                    termToOut(tn.argument)
                }
              } else {
                termToOut(tn.argument)
                if (tn.operator.head.isLetterOrDigit) out(" ")
                out(tn.operator)
              }
          }
        case tn: Sub =>
          termToOut(tn.`object`)
          out("(")
          nodeToOut(tn.property)
          out(")")
        case obj Dot name =>
          termToOut(obj)
          out(".")
          out(identifier(name))
        case Node.StaticMemberExpression(obj, prop) =>
          termToOut(obj)
          out(".")
          nodeToOut(prop)
        //case tn: Node.PropAccess => outputUnknownNode(tn)
        case tn: Node.SequenceExpression =>
          out("{\n")
          out.indent()
          for (item <- tn.expressions) {
            nodeToOut(item)
            out.eol()
          }
          out.unindent()
          out("}")
        case tn: Node.NewExpression =>
          out("new ")
          outputCall(tn.callee, tn.typeArgs, tn.arguments)
        case tn: Node.CallExpression =>
          outputCall(tn.callee, null, tn.arguments)

        case Node.VariableDeclarator(s@Node.Identifier(Id(name)), init, vType) =>
          val sType = getSymbolType(name).orElse(astType(Option(vType)).map(_ -> true))
          outputVarDef(s, Option(init), sType.map(_._1), sType.exists(_._2))

        case tn@Node.VariableDeclaration(decl, kind) =>
          outputDefinitions(kind == "const", tn)
        //case tn: Node.VariableDeclaration => outputUnknownNode(tn)
        case tn: Node.ContinueStatement =>
          val label = Option(tn.label).getOrElse("")
          out(s"/* Unsupported: Continue */ $source")
          out.eol()
        case tn: Node.BreakStatement =>
          out(s"/* Unsupported: Break */ $source")
          out.eol()
        case tn: Node.ThrowStatement =>
          out("throw")
          out(" ")
          nodeToOut(tn.argument)
          out.eol()
        case Node.ReturnStatement(argument) =>
          out("return")
          Option(argument).foreach { v =>
            out(" ")
            nodeToOut(v)
            out.eol()
          }
        //case tn: Node.Exit => outputUnknownNode(tn)
        //case tn: Node.Jump => outputUnknownNode(tn)
        case tn: Node.IfStatement =>
          out"if (${tn.test}) "
          nodeToOut(tn.consequent)
          Option(tn.alternate).foreach { a =>
            out(" else ")
            nodeToOut(a)
          }
          dumpTrailingComments(n)
          out.eol()
        case tn: Node.WithStatement => outputUnknownNode(tn, true)
        case tn: Node.ForInStatement =>

          val push = CanYield.unapply(tn)

          push.fold {
            outForHeader(tn)
            nodeToOut(tn.body)
          } { case (call, expr, arg) =>
            // transform for (a <- array) b.push(f(a)) into b ++= for (a <- array) yield f(a)
            nodeToOut(expr)
            out" ++= "

            out"${tn.right}.map { ${tn.left} =>\n"
            out.indent()

            val yieldBody = tn.body.transformBefore { (node, descend, walker) =>
              node match {
                case `call` =>
                  arg.clone()
                case _ =>
                  val c = node.clone()
                  descend(c, walker)
                  c
              }
            }

            def bodyFromStatement(s: Node.Statement): Seq[Node.StatementListItem] = {
              s match {
                case b: Node.BlockStatement =>
                  b.body
                case _ =>
                  Seq(s)
              }
            }

            blockToOut(bodyFromStatement(yieldBody))
            dumpTrailingComments(n)
            out.unindent()
            out("}")
          }
        case tn: Node.ForStatement =>
          tn match {
            case ForRange(name, until, init, end, step) =>
              (init, until, end, step) match {
                case (NumberLiteral(0), "until", expr Dot "length", NumberLiteral(1)) =>
                  out"for (${name.name} <- $expr.indices) ${tn.body}"

                case _ =>
                  out"for (${name.name} <- $init $until $end"
                  step match {
                    case NumberLiteral(1) =>
                    case _ => out" by $step"
                  }
                  out") ${tn.body}"
              }

            case _ => // generic solution using while - reliable, but ugly
              // new scope never needed in classical JS, all variables exists on a function scope
              val isScoped = Option(tn.init) match {
                case Some(Node.VariableDeclaration(_, "const" | "let")) => true
                case _ => false
              }
              if (isScoped) {
                out.eol()
                out("{\n")
                out.indent()
              }
              //out("\n\n{\n")
              Option(tn.init).foreach { init =>
                nodeToOut(init)
              }
              out.eol()
              out("while (")
              Option(tn.test).fold(out("true"))(nodeToOut)
              out(") {\n")
              out.indent()
              nodeToOut(tn.body)
              out.eol()
              Option(tn.update).foreach(nodeToOut)
              out.unindent()
              out.eol()
              out("}\n")
              if (isScoped) {
                out.unindent()
                out("}\n")
              }
            //out("}\n")
          }

        case tn: Node.WhileStatement =>
          out("while (")
          nodeToOut(tn.test)
          out(") ")
          nodeToOut(tn.body)
        case tn: Node.DoWhileStatement =>
          out("do ")
          nodeToOut(tn.body)
          out(" while (")
          nodeToOut(tn.test)
          out(")\n")
        //case tn: Node.DWLoop => outputUnknownNode(tn)
        //case tn: Node.IterationStatement => outputUnknownNode(tn)
        case tn: Node.LabeledStatement =>
          out(s"/* label ${nodeToString(tn.label)} */")
          nodeToOut(tn.body)
        //case tn: Node.StatementWithBody => outputUnknownNode(tn)
        case tn: Node.EmptyStatement =>
        case tn: Node.CatchClause =>
          out(" catch {\n")
          out.indent()
          out("case ")
          out.indent()
          nodeToOut(tn.param)
          out(" =>\n")
          blockToOut(tn.body.body)
          out.unindent()
          out.unindent()
          out("}\n")
        case tn: Node.TryStatement =>
          out("try ")
          nodeToOut(tn.block)
          Option(tn.handler).foreach(nodeToOut)
          Option(tn.finalizer).foreach { fin =>
            out(" finally ")
            blockBracedToOut(fin)
          }
        case Node.SwitchCase(null, body) =>
          out("case _ =>\n")
          out.indent()
          blockToOut(body)
          dumpTrailingComments(n)
          out.eol()
          out.unindent()

        case tn: Node.SwitchCase =>
          out("case ")

          def outputCaseBody(body: Seq[Node.StatementListItem]) = {
            out(" =>\n")
            out.indent()
            blockToOut(body)
            dumpTrailingComments(n)
            out.eol()
            out.unindent()
          }
          import Casting._
          object AsInstanceOfCondition extends InstanceOfCondition(asinstanceof)

          tn.test match {
            // CASE_CAST
            case Node.CallExpression(Node.Identifier("cast_^"), Seq(AsInstanceOfCondition(name, classes))) =>
              classes match {
                case Seq(cls) =>
                  out"${identifier(name.name + castSuffix)}: $cls"
                case _ =>
                  val matchClasses = classes.map(c => s"_: ${identifier(c.name)}").mkString(" | ")
                  out(matchClasses)
              }

              tn.consequent match {
                case Seq(block@Node.BlockStatement(Node.VariableDeclaration(Seq(Node.VariableDeclarator(sv, AsInstanceOfCondition(_, _), _)), _) +: body)) =>
                  // we might check sv - variable name correspondence
                  context.withScope(block) {
                    outputCaseBody(body)
                  }
                case _ =>
                  outputCaseBody(tn.consequent)
              }
            case _ =>
              nodeToOut(tn.test)
              outputCaseBody(tn.consequent)
          }

        //case tn: Node.SwitchCase => outputUnknownNode(tn)
        case tn: Node.SwitchStatement =>
          nodeToOut(tn.discriminant)
          out(" match {\n")
          out.indent()
          tn.cases.foreach(nodeToOut)
          out.unindent()
          out.eol()
          out("}")
        case tn: Node.FunctionDeclaration =>
          out.eol(2)
          out"def ${tn.id}"
          outputArgNames(tn.params, true)(tn)
          for (fType <- input.types.types.get(Id(tn.id.name)) if fType.certain) {
            out": ${fType.declType}"
          }
          if (tn.body != null) {
            out(" = ")
            blockBracedToOut(tn.body)
          }
          out.eol()


        /*
      case tn: Node.Accessor =>
        outputArgNames(tn, true)
        out(" = ")
        //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
        blockBracedToOut(tn.body)
        out.eol()
        */
        case tn: Node.FunctionExpression =>
          outputArgNames(tn.params)(tn)
          out(" => ")
          //out"${nodeTreeToString(tn)}:${tn.body.map(nodeClassName)}"
          blockBracedToOut(tn.body)
        case tn: Node.ArrowFunctionExpression =>
          outputArgNames(tn.params)(tn)
          out(" => ")
          tn.body match {
            case body: Node.Expression =>
              nodeToOut(body)
            case block: Node.BlockStatement =>
              blockBracedToOut(block)
          }
          out.eol()
        //case tn: Node.Program => outputUnknownNode(tn)
        //case tn: Node.Scope => outputUnknownNode(tn)

        case tn: Node.EnumDeclaration =>
          out"object ${tn.name} extends Enumeration {\n"
          out.indent()
          context.withScope(tn.body) {
            for (e <- tn.body.body) {
              // TODO: for more natural output put multiple values one a single line
              out"val ${e.name} = Value("
              for (value <- Option(e.value)) {
                out"$value"
              }
              out(")\n")
            }
          }
          out.unindent()
          out("}\n\n")

        case tn: Node.ClassDeclaration if tn.body == null =>
          out"trait ${tn.id}"
          outputTypeParameters(tn.typeParameters)
          out("\n\n")

        case tn: Node.NamespaceDeclaration =>
          out"object ${tn.id} {\n"
          context.withScope(tn.body) {
            out.indent()
            tn.body.body.foreach { i =>
              nodeToOut(i)
            }
            out.unindent()
          }
          out("}\n")

        case tn: Node.ClassDeclaration =>

          context.withScope(tn.body) {
            val (staticProperties, nonStaticProperties) = Option(tn.body).map(_.body).getOrElse(Nil).partition(propertyIsStatic)

            val isObject = staticProperties.nonEmpty || nonStaticProperties.isEmpty && tn.kind == "class"
            if (isObject) {
              out.eol(2)

              out"object ${tn.id} {\n"
              out.indent()
              staticProperties.foreach(outputMethodNode(_, "var"))
              out.unindent()
              out("}\n")

            }

            // find a constructor and output it

            val isStaticOnly = tn.superClass match {
              case Defined(Node.Identifier(`staticClassName`)) =>
                true
              case _ =>
                false
            }

            if (!isStaticOnly && !(isObject && nonStaticProperties.isEmpty && tn.superClass == null)) {
              out.eol(2)

              val kind = tn.kind match {
                case "interface" => "trait"
                case _ => "class"
              }

              out"$kind ${tn.id}"
              outputTypeParameters(tn.typeParameters)

              val inlineBodyOpt = Classes.findInlineBody(tn)

              val constructor = Classes.findConstructor(tn).map(_.value)

              for {
                inlineBody <- inlineBodyOpt
                method <- getMethodMethod(inlineBody) if method.params.nonEmpty
              } {
                outputClassArgNames(method.params)(method)
              }

              var firstExtends = true
              for (base <- Option(tn.superClass)) {
                out" extends $base"
                firstExtends = false

                for {
                  inlineBody <- inlineBodyOpt
                  method <- getMethodMethod(inlineBody)
                } {
                  // find the super constructor call and use its parameters
                  method.body.body.foreach {
                    case Node.ExpressionStatement(call@Node.CallExpression(_: Node.Super, pars)) =>
                      if (pars.nonEmpty) {
                        out("(")
                        outputNodes(pars)(nodeToOut)
                        out(")")
                      }
                    case _ =>
                  }
                }
              }

              for (base <- tn.implements) {
                val extendKeyword = if (firstExtends) "extends" else "with"
                out" $extendKeyword $base"
                firstExtends = false
              }

              out" {\n"
              out.indent()

              if (tn.body != null) dumpInnerComments(tn.body)

              for {
                inlineBody <- inlineBodyOpt
                method <- getMethodMethod(inlineBody)
              } {
                context.withScope(method, method.body) {
                  //out"/* inlineBody count ${inlineBody.value.body.length} */\n"
                  //out"/* inlineBody ${inlineBody.value.body.mkString(",")} */\n"
                  if (false) {
                    out"/* inlineBody defs ${
                      method.body.body.collect {
                        case VarDecl(vn, _, _) =>
                          vn
                      }.mkString(",")
                    } */\n"
                  }

                  // class body should be a list of variable declarations, constructor statements may follow
                  method.body.body.foreach {
                    case df: Node.VariableDeclaration =>
                      outputDefinitions(df.kind == "const", df, true)
                    case Node.ExpressionStatement(Node.CallExpression(_: Node.Super, _)) =>
                    case ss =>
                      //out(nodeTreeToString(ss))
                      nodeToOut(ss)
                  }
                }
              }

              //blockToOut(tn.body)

              val functionMembers = nonStaticProperties

              if (tn.body.body.nonEmpty && constructor.nonEmpty) out.eol(2)

              if (constructor.nonEmpty && functionMembers.nonEmpty) out.eol(2)

              var separateVarAndFunction = false

              for (pm <- functionMembers if !inlineBodyOpt.contains(pm)) {
                dumpLeadingComments(pm)
                pm match {
                  case p: Node.MethodDefinition =>
                    // check overrides
                    def isObjectOverride = {
                      // special case: override AnyRef (java.lang.Object) methods:
                      val objectMethods = Set("clone", "toString", "hashCode", "getClass")
                      getMethodMethod(p).exists(_.params.isEmpty) && objectMethods.contains(propertyKeyName(p.key))
                    }

                    def isNormalOverride = {
                      val isOverride = for {
                        parentSym <- Classes.superClass(tn)
                        parentCls <- input.classes.get(parentSym)
                        parentMethod <- Classes.findMethod(parentCls, propertyKeyName(p.key))
                      } yield {
                        // check method signature
                        def getArgTypes(m: Node.MethodDefinition) = {
                          val paramsNames = m.value match {
                            case AnyFun(params, _) =>
                              params.map(parameterNameString)
                          }
                          val scopeId = ScopeContext.getNodeId(p.value)
                          val paramIds = paramsNames.map(SymId(_, scopeId))
                          paramIds.map(id => input.types.get(Some(id)))
                        }

                        val myParTypes = getArgTypes(p)
                        val parentTypes = getArgTypes(parentMethod)
                        myParTypes == parentTypes
                      }
                      isOverride.contains(true)
                    }

                    val isFunction = AnyFun.unapply(p.value).isDefined

                    out.eol(if (separateVarAndFunction && isFunction) 2 else 1)
                    separateVarAndFunction = false

                    if (isObjectOverride || isNormalOverride) out("override ")
                    outputMethodNode(p, eol = false)
                    if (isFunction) out("\n")
                    separateVarAndFunction = !isFunction

                  case _ =>
                    nodeToOut(pm)
                }
              }

              // ignore trailing comments, they are unlikely to belong to class
              // class scope is introduces by us, traling comment is most likely a leading comment of the following item

              out.unindent()
              out.eol()
              out("}\n\n")
              // classes have no body
              //blockBracedToOut(tn.body)
            }
          }


        case tn: Node.BlockStatement =>
          blockBracedToOut(tn)
        //case tn: Node.BlockStatement =>
        case tn: Node.ExpressionStatement =>
          nodeToOut(tn.expression)
          dumpInnerComments(n)
          out.eol()
        case tn: ScalaNode.StatementExpression =>
          nodeToOut(tn.statement)
        case tn: Node.Directive =>
          if (source != """"use strict";""" && source != "'use strict';") { // east use strict silently
            outputUnknownNode(tn)
            out.eol()
          }
        case tn: Node.DebuggerStatement =>
          outputUnknownNode(tn)
          out.eol()
      case tn: Node.ExportNamedDeclaration =>
        if (tn.declaration != null) {
          out("/* export */ ")
          nodeToOut(tn.declaration)
        } else {
          out(s"/* ${trimSource(source)} */")
        }
      case tn: Node.ExportDefaultDeclaration =>
        Option(tn.declaration).foreach(nodeToOut)
        out(s"/* ${trimSource(source)} */")
      case tn: Node.ExportAllDeclaration =>
        //out(s"/* export */ def ${tn.exported_definition} name ${tn.module_name} value ${tn.exported_value}\n")
        out(s"/* ${trimSource(source)} */")
      case tn: Node.ImportDeclaration =>
        // try to create a package name from the import directive
        // start from the root
        val imported_names = tn.specifiers.flatMap {
          case i: Node.ImportNamespaceSpecifier =>
            Some(i.local.name)
          case i: Node.ImportDefaultSpecifier =>
            Some(i.local.name)
          case i: Node.ImportSpecifier =>
            Some(i.imported.name)
        }
        val moduleFile: String = tn.source.value
        val moduleName = if (moduleFile.endsWith(".js")) moduleFile.dropRight(2) else moduleFile
        val toOut = outConfig.formatImport(imported_names, moduleName, source)
        out(toOut)
        case tn =>
          outputUnknownNode(tn)
          out.eol()
      }


    }

  }

  private val scalaFirst = "?".toSet

  private def identifier(name: String) = {
    def wrapped = "`" + name + "`"
    def acceptableFirstChar(c: Char) = {
      java.lang.Character.isJavaIdentifierStart(c) || scalaFirst.contains(c)
    }
    def acceptableNextChar(c: Char) = {
      java.lang.Character.isJavaIdentifierPart(c) || scalaFirst.contains(c)
    }
    if (Keywords(name) || name.isEmpty) {
      wrapped
    } else if (!acceptableFirstChar(name(0)) || !name.tail.forall(acceptableNextChar)) {
      wrapped
    } else {
      name
    }
  }

  private def identifierToOut(out: Output, name: String) = {
    out(identifier(name))
  }

  private def blockBracedToOut(block: Node.BlockStatement, force: Boolean = false)(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext) = {
    val body = block.body
    // TODO: single statement without braces
    out("{")
    dumpLeadingComments(block)
    out.eol()
    out.indent()
    blockToOut(body)
    dumpInnerComments(block)
    out.unindent()
    out.eol()
    out("}")
  }

  private def blockToOut(body: Seq[Node.StatementListItem])(implicit outConfig: Config, input: InputContext, out: Output, context: ScopeContext): Unit = {
    for ((s, notLast) <- markEnd(body)) {
      dumpLeadingComments(s)
      nodeToOut(s)
      if (notLast) out.eol()
      else dumpTrailingComments(s)
    }
  }

  def output(ast: NodeExtended, input: String, outConfig: Config = Config.default): Seq[String] = {
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
        if (!isIndented) {
          // check if we have crossed a file boundary, start a new output file if needed
          //println(s"loc $loc of ${outConfig.parts}")
          while (currentSb < outConfig.parts.length && loc >= outConfig.parts(currentSb)) {
            currentSb += 1
            //println(s"Advance to $currentSb at $loc - debug $debug")
          }
        }
      }
    }

    val classListHarmony = ClassListHarmony.fromAST(ast.top)
    val inputContext = InputContext(input, ast.types, classListHarmony)
    val scopeContext = new ScopeContext
    scopeContext.withScope(ast.top) {
      blockToOut(ast.top.body)(outConfig, inputContext, ret, scopeContext)
      ret.flush()
    }
    sb.map(_.result())
  }

  def nodeSource(n: Node.Node, input: String): String = {
    (for {
      s <- n.start
      e <- n.end
    } yield input.slice(s, e)).getOrElse("")
  }

  def outputNode(ast: Node.Node, input: String = "", outConfig: Config = Config.default): String = {
    val sb = new StringBuilder
    val ret = new NiceOutput {
      def out(x: String) = sb append x
    }
    val classListEmpty = ClassListHarmony.empty
    val inputContext = InputContext(input, SymbolTypes(), classListEmpty)
    val scopeContext = new ScopeContext
    nodeToOut(ast)(outConfig, inputContext, ret, scopeContext)
    ret.flush()
    sb.result()
  }
}