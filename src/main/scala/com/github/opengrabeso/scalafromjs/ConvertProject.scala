package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import CommandLine._

import scala.util.matching.Regex
//import com.github.opengrabeso.Transform.NodeExtended

import scala.collection.immutable.ListMap
import scala.collection.mutable
import ConvertProject._
import PathUtils._

import scala.reflect.ClassTag

object ConvertProject {

  def loadStringValue(o: OObject, name: String): Option[String] = {
    o.properties.collectFirst {
      case ObjectKeyVal(`name`, StringLiteral(value)) =>
        value
      case ObjectKeyVal(`name`, AArray(lines)) =>
        val lineStrings = lines.collect {
          case StringLiteral(s) => s
        }
        lineStrings.mkString("\n")
    }
  }

  def loadRequiredStringValue(o: OObject, name: String): String = {
    val opt = loadStringValue(o, name)
    opt.getOrElse {
      throw new UnsupportedOperationException(s"Missing entry '$name'")
    }
  }
  trait Rule {
    def apply(c: NodeExtended): NodeExtended
  }

  /**
    * Use for rules which perform no AST processsing
    * Such rules carry the information alongside with the AST
    * they are called separately by using collectRules and calling their specific methods
    * */
  trait ExternalRule extends Rule {
    override def apply(c: NodeExtended) = c
  }

  case class MemberDesc(cls: Regex, name: Regex)

  object MemberDesc {
    def load(o: OObject): MemberDesc = {
      val cls = new Regex(loadRequiredStringValue(o, "cls"))
      val name = new Regex(loadRequiredStringValue(o, "name"))
      MemberDesc(cls, name)
    }
  }

  case class DeleteMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.deleteMembers(n, member)
    }
  }

  case class IsClassMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.replaceIsClass(n, member)
    }
  }

  case class GetClassMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.replaceGetClass(n, member)
    }
  }

  case class MakePropertyRule(member: MemberDesc) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.makeProperties(n, member)
    }
  }

  case class ReplicateMemberRule(member: MemberDesc, template: String) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.substMember(n, member, template)
    }

  }

  case class RemoveScopeRule(scope: List[String]) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.removeScope(n, scope)
    }
  }

  case class AliasPackageRule(folder: String, name: String, template: Option[String]) extends ExternalRule {
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
    def load(o: OObject): AliasPackageRule = {
      val folder = loadRequiredStringValue(o, "folder")
      val name = loadRequiredStringValue(o, "name")
      val template = loadStringValue(o, "template")
      AliasPackageRule(terminatedPath(folder), terminatedPath(name), template)
    }
  }

  case class RegexPostprocessRule(find: String, replace: String) extends ExternalRule {
    def transformText(src: String): String = src.replaceAll(find, replace)
  }

  case class RegexPreprocessRule(find: String, replace: String) extends ExternalRule {
    def transformText(src: String): String = src.replaceAll(find, replace)
  }

  val configName = "ScalaFromJS_settings"

  case class ConvertConfig(rules: Seq[Rule] = Seq.empty) {
    def collectRules[T: ClassTag]: Seq[T] = rules.collect {case x: T => x}

    def postprocess(src: String): String = {
      val processRules = collectRules[RegexPostprocessRule]
      processRules.foldLeft(src)((processed, rule) => rule.transformText(processed))
    }
    def preprocess(src: String): String = {
      val processRules = collectRules[RegexPreprocessRule]
      processRules.foldLeft(src)((processed, rule) => rule.transformText(processed))
    }
  }

  object ConvertConfig {

    def load(props: Seq[Node.ObjectExpressionProperty], root: Option[String]) = {
      val rules: Seq[Rule] = props.flatMap {
        case ObjectKeyVal("members", a: AArray) =>
          a.elements.flatMap {
            case o: OObject =>
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
        case ObjectKeyVal("packages", a: AArray) =>
          a.elements.flatMap {
            case o: OObject =>
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
        case ObjectKeyVal("symbols", a: AArray) =>
          a.elements.flatMap {
            case o: OObject =>
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
        case ObjectKeyVal("postprocess", a: AArray) =>
          a.elements.flatMap {
            case o: OObject =>
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
        case ObjectKeyVal("preprocess", a: AArray) =>
          a.elements.flatMap {
            case o: OObject =>
              val op = loadStringValue(o, "operation")
              op match {
                case Some("replace") =>
                  val find = loadRequiredStringValue(o, "pattern")
                  val replace = loadRequiredStringValue(o, "replace")
                  Some(RegexPreprocessRule(find, replace))
                case _ =>
                  None
              }
            case _ =>
              None
          }
        case ObjectKeyVal("types", StringLiteral(types)) =>
          root.map(r => transform.TypesRule(types, r))
        case ObjectKeyVal(name, _) =>
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
    val terminatedCode = if (code.lastOption.contains('\n')) code else code + "\n"
    terminatedCode
  }
  case class Item(code: String, included: Boolean, fullName: String) {
    assert(!fullName.contains("../")) // must not contain .., because resolveSibling cannot handle it
    override def toString = s"($fullName:$included)"
  }

  def loadControlFile(in: String): ConvertProject = {

    // parse only the control file to read the preprocess rules
    val inSource = readSourceFile(in)
    val ext = NodeExtended(parse(inSource)).loadConfig(Some(in)).config

    Time("loadControlFile") {
      val code = ext.preprocess(inSource)
      val project = ConvertProject(in, ext.preprocess, ListMap(in -> Item(code, true, in)))

      project.resolveImportsExports
    }
  }

  def loadConfig(ast: Node.Program, root: Option[String]): (ConvertConfig, Node.Program) = {
    var readConfig = Option.empty[ConvertConfig]

    object GetConfig {
      def unapply(arg: Node.Node) = arg match {
        case VarDecl(`configName`, Some(OObject(props)), _) =>
          Some(props)
        case _ =>
          None
      }
    }

    ast.walk {
      case GetConfig(props) =>
        readConfig = Some(ConvertConfig.load(props, root))
        false
      case _: Node.Program =>
        false
      case IsScope() =>
        true // do not descend into any other scopes, we expect the config at the top level only
      case _ =>
        false

    }

    val removedConfig = readConfig.fold(ast) { rc =>
      ast.transformAfter { (node, _) =>
        node match {
          case GetConfig(_) =>
            Node.EmptyStatement()
          case _ =>
            node
        }

      }
    }

    readConfig.getOrElse(ConvertConfig()) -> removedConfig
  }


}


case class ConvertProject(root: String, preprocess: String => String, items: Map[String, Item]) {
  val values = items.values.toIndexedSeq
  lazy val code = values.map(_.code).mkString
  lazy val offsets = values.scanLeft(0)((offset, file) => offset + file.code.length)

  def checkIntegrity = {
    val missingEol = values.filterNot(_.code.endsWith("\n"))
    missingEol.map(_.fullName).foreach(s => println(s"Missing eol in $s"))
    val wrongPath = values.filter(i => i.fullName contains "../")
    wrongPath.map(_.fullName).foreach(s => println(s"Wrong path in $s"))
    missingEol.isEmpty && wrongPath.isEmpty
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
      val code = preprocess(readSourceFile(path))
      // try parsing, if unable, return a comment file instead
      try {
        val typescript = PathUtils.extension(PathUtils.shortName(path)) == "ts"
        parse(code, typescript)
        code -> path
      } catch {
        case ex: Exception =>
          //println(s"Parse ex: ${ex.toString} in $path")
          val short = shortName(path)
          val dot = short.indexOf('.')
          val simpleName = if (dot < 0) short else short.take(dot)
          // never attempt to wrap source files as resources
          // note: this is no longer necessary for Three.js, as they have already wrapped the sources as glsl.js files
          // it may be handy for other projects, though
          val neverWrap = Seq(".js", ".ts", ".d.ts")
          if (dot >= 0 && neverWrap.contains(short.drop(dot))) {
            throw ex
          } else {
            // embed wrapped code as a variable using ES6 template string
            // use isResource so that Scala output can check it and handle it as a special case
            val wrap =
            s"""
               |const $simpleName = {
               |  value: `$code`,
               |  isResource: true
               |}
               |""".stripMargin
            wrap -> path
          }
      }

    }


    /**@return (content, path) */
    def readJsFile(name: String): (String, String) = {
      // imports often miss .js or .d.ts extension
      val rootExtension = PathUtils.shortName(root).dropWhile(_ != '.')
      val extension = if (rootExtension.nonEmpty) rootExtension else ".js"
      // first try if it is already loaded
      items.get(name).orElse(items.get(name + extension)).fold {
        //println(s"Read file $name as $singlePath (in $base)")
        try {
          readFileAsJs(name)
        } catch {
          case _: java.io.FileNotFoundException if !name.endsWith(extension) =>
            readFileAsJs(name + extension)
        }
      } { item =>
        item.code -> item.fullName
      }
    }

    val ast = try {
      //println("** Parse\n" + items.mkString("\n"))
      val typescript = PathUtils.extension(PathUtils.shortName(root)) == "ts"
      parse(code, typescript)
    } catch {
      case ex: Exception =>
        println(s"Parse error $ex")
        /*
        println(s"file ${err.filename} at ${err.line}:${err.col}")

        val context = code.slice(err.pos - 30, err.pos + 30)
        println(s"Context: \n$context")
        */
        throw ex
    }

    // check export / import statements
    val exampleBuffer = new mutable.ArrayBuffer[(String, String)]
    val includeBuffer = new mutable.ArrayBuffer[(String, String)]
    ast.walk {
      case i: Node.ImportDeclaration =>
        val example = Option(i.leadingComments).toSeq.flatten.exists { commentToken =>
          val comment = commentToken.value
          comment contains "@example"
        }
        val target = if (example) exampleBuffer else includeBuffer
        val inFile = pathForOffset(i.range._1)
        val importPath = i.source.value
        target append readJsFile(resolveSibling(inFile, importPath))
        false
      case e@ExportFromSource(Defined(StringLiteral(source))) =>
        // ignore exports in imported files
        e.start.foreach { s =>
          val index = indexOfItem(s)
          //println(s"index of ${s.pos} = $index in $offsets")
          if (values.isDefinedAt(index) && values(index).included) {
            val inFile = pathForOffset(s)
            includeBuffer append readJsFile(resolveSibling(pathForOffset(s), source))
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

      ConvertProject(root, preprocess, old ++ examples ++ includes).resolveImportsExports
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
          val typescript = PathUtils.extension(PathUtils.shortName(name)) == "ts"
          parse(code, typescript)
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

    val ast = Time(s"Parse ${compositeFile.linesIterator.length} lines") {
      val typescript = PathUtils.extension(PathUtils.shortName(root)) == "ts"
      parse(compositeFile, typescript)
    }

    val ext = NodeExtended(ast).loadConfig(Some(root))

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
