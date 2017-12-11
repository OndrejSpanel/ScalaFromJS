package com.github.opengrabeso

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import CommandLine._
import com.github.opengrabeso.Transform.AST_Extended

import scala.collection.immutable.ListMap
import scala.collection.mutable
import ConvertProject._
import PathUtils._

import scala.reflect.ClassTag

object ConvertProject {

  def loadStringValue(o: AST_Object, name: String): Option[String] = {
    o.properties.collectFirst {
      case AST_ObjectKeyVal(`name`, AST_String(value)) =>
        value
      case AST_ObjectKeyVal(`name`, AST_Array(lines@_*)) =>
        val lineStrings = lines.collect {
          case s: AST_String => s.value
        }
        lineStrings.mkString("\n")
    }
  }

  def loadRequiredStringValue(o: AST_Object, name: String): String = {
    val opt = loadStringValue(o, name)
    opt.getOrElse {
      throw new UnsupportedOperationException(s"Missing entry '$name'")
    }
  }
  trait Rule {
    def apply(c: AST_Extended): AST_Extended
  }

  case class MemberDesc(cls: RegExp, name: RegExp)

  object MemberDesc {
    def load(o: AST_Object): MemberDesc = {
      val cls = RegExp(loadRequiredStringValue(o, "cls"))
      val name = RegExp(loadRequiredStringValue(o, "name"))
      MemberDesc(cls, name)
    }
  }

  case class DeleteMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      transform.classes.Rules.deleteMembers(n, member)
    }
  }

  case class IsClassMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      transform.classes.Rules.replaceIsClass(n, member)
    }
  }

  case class GetClassMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      transform.classes.Rules.replaceGetClass(n, member)
    }
  }

  case class MakePropertyRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      transform.classes.Rules.makeProperties(n, member)
    }
  }

  case class ReplicateMemberRule(member: MemberDesc, template: String) extends Rule {
    override def apply(n: AST_Extended) = {
      transform.classes.Rules.substMember(n, member, template)
    }

  }

  case class RemoveScopeRule(scope: List[String]) extends Rule {
    override def apply(n: AST_Extended) = {
      transform.classes.Rules.removeScope(n, scope)
    }
  }

  case class AliasPackageRule(folder: String, name: String, template: Option[String]) extends Rule {
    override def apply(n: AST_Extended) = n
    def namePackage(path: String): Option[String] = {
      if (path startsWith folder) {
        val aliased = name ++ path.drop(folder.length)
        //println(s"aliased $path -> $aliased")
        Some(aliased)
      }
      else None
    }
    def applyTemplate(shortName: String, content: String): String = {
      template.fold(content){ t =>
        import TextTemplates._
        val dotIndex = shortName.indexOf('.')
        val className = if (dotIndex < 0) shortName else shortName.take(dotIndex)
        t.substitute("class", className).substitute("this", content)
      }
    }
  }

  object AliasPackageRule {
    def load(o: AST_Object): AliasPackageRule = {
      val folder = loadRequiredStringValue(o, "folder")
      val name = loadRequiredStringValue(o, "name")
      val template = loadStringValue(o, "template")
      AliasPackageRule(terminatedPath(folder), terminatedPath(name), template)
    }
  }

  /**
    * The real functionality of the regex rule is outside of AST processing, as a text only postprocess
    * */
  case class RegexPostprocessRule(find: String, replace: String) extends Rule {
    def apply(c: AST_Extended): AST_Extended = c

    def transformText(src: String): String = {
      src.replaceAll(find, replace)
    }
  }

  val configName = "ScalaFromJS_settings"

  case class ConvertConfig(rules: Seq[Rule] = Seq.empty) {
    def collectRules[T: ClassTag]: Seq[T] = rules.collect {case x: T => x}

    def postprocess(src: String): String = {
      val postRules = collectRules[RegexPostprocessRule]
      postRules.foldLeft(src)((processed, rule) => rule.transformText(processed))
    }
  }

  object ConvertConfig {

    def load(props: Seq[AST_ObjectProperty]) = {
      val rules: Seq[Rule] = props.flatMap {
        case AST_ObjectKeyVal("members", a: AST_Array) =>
          a.elements.toSeq.flatMap {
            case o: AST_Object =>
              val m = MemberDesc.load(o)
              val op = loadStringValue(o, "operation")
              op match {
                case Some("delete") =>
                  Some(DeleteMemberRule(m))
                case Some("make-property") =>
                  Some(MakePropertyRule(m))
                case Some("instanceof") =>
                  Some(IsClassMemberRule(m))
                case Some("getClass") =>
                  Some(GetClassMemberRule(m))
                case Some("subst") =>
                  val template = loadRequiredStringValue(o, "template")
                  Some(ReplicateMemberRule(m, template))
                case Some(opName) =>
                  throw new UnsupportedOperationException(s"Unknown operation $opName for member $m")
                case _ =>
                  throw new UnsupportedOperationException(s"Missing operation for member $m")
              }
            case _ =>
              None
          }
        case AST_ObjectKeyVal("packages", a: AST_Array) =>
          a.elements.toSeq.flatMap {
            case o: AST_Object =>
              val op = loadStringValue(o, "operation")
              val folder = loadStringValue(o, "folder")
              op match {
                case Some("name") =>
                  Some(AliasPackageRule.load(o))
                case Some(opName) =>
                  throw new UnsupportedOperationException(s"Unknown operation $opName for folder $folder")
                case _ =>
                  throw new UnsupportedOperationException(s"Missing operation for folder $folder")
              }
            case _ =>
              None
          }
        case AST_ObjectKeyVal("symbols", a: AST_Array) =>
          a.elements.toSeq.flatMap {
            case o: AST_Object =>
              val op = loadStringValue(o, "operation")
              //println(s"op $op")
              val name = loadRequiredStringValue(o, "name")
              //println(s"name $name")
              op match {
                case Some("remove") =>
                  val symbol = loadRequiredStringValue(o, "name").split("/")
                  if (symbol.nonEmpty) {
                    Some(RemoveScopeRule(symbol.toList))
                  } else {
                    None
                  }
                case _ =>
                  throw new UnsupportedOperationException(s"Missing operation for symbol $name")
              }
            case _ =>
              None
          }
        case AST_ObjectKeyVal("postprocess", a: AST_Array) =>
          a.elements.toSeq.flatMap {
            case o: AST_Object =>
              val op = loadStringValue(o, "operation")
              op match {
                case Some("replace") =>
                  val find = loadRequiredStringValue(o, "pattern")
                  val replace = loadRequiredStringValue(o, "replace")
                  Some(RegexPostprocessRule(find, replace))
                case _ =>
                  None
              }
            case _ =>
              None
          }
        case AST_ObjectKeyVal(name, _) =>
          throw new UnsupportedOperationException(s"Unexpected config entry $name")
        case n =>
          throw new UnsupportedOperationException(s"Unexpected config entry of type ${nodeClassName(n)}")
      }
      //println("Rules " + rules)
      ConvertConfig(rules)
    }
  }

  def readSourceFile(in: String): String = {
    val code = readFile(in)
    val terminatedCode = if (code.last == '\n') code else code + "\n"
    terminatedCode
  }
  case class Item(code: String, included: Boolean, fullName: String) {
    override def toString = s"($fullName:$included)"
  }

  def loadControlFile(in: String): ConvertProject = {
    Time("loadControlFile") {
      val code = readSourceFile(in)
      val project = ConvertProject(in, ListMap(in -> Item(code, true, in)))

      project.resolveImportsExports
    }
  }

  def loadConfig(ast: AST_Toplevel): (ConvertConfig, AST_Toplevel) = {
    var readConfig = Option.empty[ConvertConfig]

    object GetConfig {
      def unapply(arg: AST_Node) = arg match {
        case AST_Definitions(AST_VarDef(AST_SymbolName(`configName`), AST_Object(props))) =>
          Some(props)
        case _ =>
          None
      }
    }

    ast.walk {
      case GetConfig(props) =>
        readConfig = Some(ConvertConfig.load(props))
        false
      case _: AST_Toplevel =>
        false
      case _: AST_Scope =>
        true // do not descend into any other scopes, we expect the config at the top level only
      case _ =>
        false

    }

    val removedConfig = readConfig.fold(ast) { rc =>
      ast.transformAfter { (node, _) =>
        node match {
          case GetConfig(_) =>
            AST_EmptyStatement(node)
          case _ =>
            node
        }

      }
    }

    readConfig.getOrElse(ConvertConfig()) -> removedConfig
  }


}


case class ConvertProject(root: String, items: Map[String, Item]) {
  lazy val values = items.values.toIndexedSeq
  lazy val code = values.map(_.code).mkString
  lazy val offsets = values.scanLeft(0)((offset, file) => offset + file.code.length)

  def checkIntegrity = {
    val missingEol = values.filterNot(_.code.endsWith("\n"))
    missingEol.map(_.fullName).foreach(s => println(s"Missing eol in $s"))
    missingEol.isEmpty
  }

  assert(checkIntegrity)

  def indexOfItem(offset: Int): Int = offsets.prefixLength(_ <= offset) - 1

  def pathForOffset(offset: Int): String = {
    val index = indexOfItem(offset)
    if (values.isDefinedAt(index)) values(index).fullName
    else ""
  }

  final def resolveImportsExports: ConvertProject = {
    def readFileAsJs(path: String): (String, String) = {
      val code = readSourceFile(path)
      // try parsing, if unable, return a comment file instead
      try {
        minify(code, defaultUglifyOptions).top
        code -> path
      } catch {
        case JavaScriptException(ex) if ex.asInstanceOf[js.Dynamic].name.asInstanceOf[String]=="SyntaxError" =>
          //println(s"Parse ex: ${ex.toString} in $path")
          val short = shortName(path)
          val dot = short.indexOf('.')
          val simpleName = if (dot <0) short else short.take(dot)
          // embed wrapped code as a variable using ES6 template string
          // use isResource so that Scala output can check it and handle it as a special case
          val wrap =
            s"""
               |var $simpleName = {
               |  value: `$code`,
               |  isResource: true
               |}
               |""".stripMargin
          wrap -> path
      }

    }


    /**@return (content, path) */
    def readJsFile(name: String): (String, String) = {
      // first try if it is already loaded

      // imports often miss .js extension
      val extension = ".js"
      items.get(name).orElse(items.get(name + extension)).fold {
        //println(s"Read file $name as $singlePath (in $base)")
        try {
          readFileAsJs(name)
        } catch {
          case ex@js.JavaScriptException(ErrorCode("ENOENT" | "EISDIR")) if !name.endsWith(extension) =>
            readFileAsJs(name + extension)
        }
      } { item =>
        item.code -> item.fullName
      }
    }

    val ast = try {
      //println("** Parse\n" + items.mkString("\n"))
      minify(code, defaultUglifyOptions).top
    } catch {
      case ex@JavaScriptException(exJS) if exJS.asInstanceOf[js.Dynamic].name.asInstanceOf[String]=="SyntaxError" =>
        val err = exJS.asInstanceOf[JS_Parse_Error]
        println(s"Parse error $err")
        println(s"file ${err.filename} at ${err.line}:${err.col}")

        val context = code.slice(err.pos - 30, err.pos + 30)
        println(s"Context: \n$context")
        throw ex
    }

    // check export / import statements
    val exampleBuffer = new mutable.ArrayBuffer[(String, String)]
    val includeBuffer = new mutable.ArrayBuffer[(String, String)]
    ast.walk {
      case i: AST_Import =>
        i.start.foreach { s =>
          val example = s.comments_before.exists {commentToken =>
            val comment = commentToken.value.asInstanceOf[String]
            comment contains "@example"
          }
          val target = if (example) exampleBuffer else includeBuffer
          target append readJsFile(resolveSibling(pathForOffset(s.pos), i.module_name.value))
        }
        false
      case e: AST_Export =>
        // ignore exports in imported files
        e.start.foreach { s =>
          val index = indexOfItem(s.pos)
          //println(s"index of ${s.pos} = $index in $offsets")
          if (values.isDefinedAt(index) && values(index).included) {
            e.module_name.foreach { name =>
              includeBuffer append readJsFile(resolveSibling(pathForOffset(s.pos), name.value))
            }
          }
        }

        false
      case _ =>
        false
    }

    // check if there are new items added into the project
    val toExample = exampleBuffer.filter(name => !items.contains(name._2))
    val toInclude = includeBuffer.filter(name => !items.contains(name._2))
    val markAsIncludes = includeBuffer.filter(name => items.get(name._2).exists(!_.included)).toSet

    if (toExample.isEmpty && toInclude.isEmpty && markAsIncludes.isEmpty) {
      //println(s"Do not add to ${items.map(_.name).mkString(",")}")
      this
    } else {

      if (false) {
        println(s"Add to ${items.mapValues(_.fullName).mkString("(", "\n", ")")}")
        println(s"  examples ${toExample.map(_._2).mkString("(", "\n", ")")}")
        println(s"  includes ${toInclude.map(_._2).mkString("(", "\n", ")")}")
        println(s"  markAsIncludes ${markAsIncludes.mkString("(", "\n", ")")}")
      }

      def mapFile(cp: (String, String), exported: Boolean): (String, Item) = {
        val (code, path) = cp
        path -> Item(code, exported, path)
      }

      val old = items.mapValues(i => i.copy(included = i.included || markAsIncludes.exists(_._2 == i.fullName)))
      val examples = toExample.map(nb => mapFile(nb, false))
      val includes = toInclude.map(nb => mapFile(nb, true))

      assert((old.keySet intersect examples.map(_._1).toSet).isEmpty)
      assert((old.keySet intersect includes.map(_._1).toSet).isEmpty)
      //println(s"  old ${old.map(_.name).mkString(",")}")

      ConvertProject(root, old ++ examples ++ includes).resolveImportsExports
    }
  }

  case class Converted(files: Seq[(String, String)], config: ConvertConfig)

  def convert: Converted = {
    val exportsImports = values.sortBy(!_.included)
    //println(s"exportsImports ${exportsImports.map(_.copy(code = ""))}")

    if (false) { // debugging the parse - parse files one by one to pinpoint a problem location
      for (ConvertProject.Item(code, _, name) <- exportsImports) {
        try {
          println(s"Parse $name")
          minify(code, defaultUglifyOptions).top
        } catch {
          case util.control.NonFatal(ex) =>
            ex.printStackTrace()
        }
      }
    }

    val exports = exportsImports.takeWhile(_.included)

    val fileOffsets = exports.scanLeft(0)((offset, file) => offset + file.code.length)
    //println(fileOffsets.drop(1) zip project.exports)

    val compositeFile = exportsImports.map(_.code).mkString

    val ast = Time(s"Parse ${compositeFile.lines.length} lines") {
      minify(compositeFile, defaultUglifyOptions).top
    }

    val ext = AST_Extended(ast).loadConfig

    val astOptimized = if (true) Transform(ext) else ext

    val outConfig = ScalaOut.Config().withParts(fileOffsets drop 1).withRoot(root)
    //println(s"$outConfig")
    val output = ScalaOut.output(astOptimized, compositeFile, outConfig)

    if (false) {
      (fileOffsets zip exports).foreach { case (offset, filename) =>
        println(s"  Offset $offset filename ${filename.fullName}")
      }
    }

    val outFiles = for ( (outCode, ConvertProject.Item(_, _, inFile)) <- output zip exports) yield {
      inFile -> outCode
    }

    Converted(outFiles, ext.config)
  }
}
