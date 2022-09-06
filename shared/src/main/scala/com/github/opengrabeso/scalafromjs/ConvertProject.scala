package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import CommandLine._
import FileAccess._

import scala.util.matching.Regex
//import com.github.opengrabeso.Transform.NodeExtended

import scala.collection.immutable.ListMap
import scala.collection.mutable
import ConvertProject._
import PathUtils._

import scala.reflect.ClassTag
import scala.collection.Seq

object ConvertProject {

  implicit class StripSource(s: String) {
    // TODO: consider custom stripMargin acting like Java 13 instead
    def stripSource: String = s.split('\n').map(_.trim).mkString("\n")
  }

  val predefinedHeadersTS = collection.immutable.Seq(
    "ArrayLike.d.ts" ->
      """
      export interface ArrayLike<T> {
        [n: number]: T;
        length: number;
      }
      """.stripSource
  )
  val predefinedHeadersJS = Nil

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

  case class Hints(
    literals: Option[String] = None
  ) {
    def append(that: Hints): Hints = {
      Hints(
        literals.orElse(that.literals)
      )
    }
  }
  case class HintsRule(path: String, hints: Hints) extends Rule {
    override def apply(n: NodeExtended) = n
  }

  case class RemoveScopeRule(scope: List[String]) extends Rule {
    override def apply(n: NodeExtended) = {
      transform.classes.Rules.removeScope(n, scope)
    }
  }

  /**
    * @param template substitution template applied on the Scala result (after conversion)
    * @param jsTemplate substitution template applied on the JS source (before conversion)
    * */
  case class AliasPackageRule(folder: String, name: String, template: Option[String], jsTemplate: Option[String]) extends ExternalRule {
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
    def applyJsTemplate(shortName: String, content: String): String = {
      jsTemplate.fold(content){ t =>
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
      val jsTemplate = loadStringValue(o, "jsTemplate")
      AliasPackageRule(terminatedPath(folder), terminatedPath(name), template, jsTemplate)
    }
  }

  trait RegExTransformRule extends ExternalRule {
    def files: Option[String]
    def find: String
    def replace: String

    def transformText(path: String, src: String): String = {
      if (files.forall(path.matches)) {
        src.replaceAll(find, replace)
      } else src
    }
  }
  case class RegexPostprocessRule(find: String, replace: String, files: Option[String]) extends RegExTransformRule

  case class RegexPreprocessRule(find: String, replace: String, files: Option[String]) extends RegExTransformRule

  val prefixName = "ScalaFromJS_"
  val configName = prefixName + "settings"


  case class ConvertConfig(root: String = "", rules: Seq[Rule] = Seq.empty) {
    def collectRules[T: ClassTag]: Seq[T] = rules.collect {case x: T => x}

    def postprocess(path: String, src: String): String = {
      val processRules = collectRules[RegexPostprocessRule]
      processRules.foldLeft(src)((processed, rule) => rule.transformText(path, processed))
    }
    def preprocess(path: String, src: String): String = {
      val processRules = collectRules[RegexPreprocessRule]
      val ret = processRules.foldLeft(src)((processed, rule) => rule.transformText(path, processed))
      ret
    }

    def hintsForPath(path: String): Hints = {
      val hintsRules = collectRules[HintsRule]
      val allHints = hintsRules.foldLeft(Hints()) { (hints, rule) =>
        // first match applies
        if (path.matches(rule.path)) {
          hints append rule.hints
        } else hints
      }
      allHints
    }

    def findAlias(filePath: String): (String, Option[AliasPackageRule]) = {
      val inRelativePathIndex = filePath.lastIndexOf('/')
      val inRelativePath = if (inRelativePathIndex < 0) "" else filePath.take(inRelativePathIndex)
      val terminated = terminatedPath(inRelativePath)
      for (alias <- collectRules[AliasPackageRule]) {
        val named = alias.namePackage(terminated)
        if (named.isDefined) {
          return (named.get, Some(alias))
        }
      }
      (filePath.reverse.dropWhile(_ != '/').drop(1).reverse, None)
    }

    def handleAliasPreprocess(absPath: String)(content: String): String = {
      val filePath = relativePath(root, absPath)

      val shortFileName = shortName(filePath)
      val (name, alias) = findAlias(filePath)
      // package name does not matter here, return the content only
      val ret = alias.map(_.applyJsTemplate(shortFileName, content)).getOrElse(content)
      ret
    }

    def handleAlias(filePath: String)(content: String): (String, String) = {
      val shortFileName = shortName(filePath)
      val (name, alias) = findAlias(filePath)
      val newContent = alias.map(_.applyTemplate(shortFileName, content)).getOrElse(content)
      // return the package name
      (name, newContent)
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
                  val files = loadStringValue(o, "files")
                  Some(RegexPostprocessRule(find, replace, files))
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
                  val files = loadStringValue(o, "files")
                  Some(RegexPreprocessRule(find, replace, files))
                case _ =>
                  None
              }
            case _ =>
              None
          }
        case ObjectKeyVal("hints", a: AArray) =>
          a.elements.flatMap {
            case o: OObject =>
              val files = loadRequiredStringValue(o, "path")
              Some(HintsRule(files, Hints(
                literals = loadStringValue(o, "literals")
              )))
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
      ConvertConfig(root.getOrElse(""), rules)
    }
  }

  def detectTypescript(in: String): Boolean = PathUtils.extension(PathUtils.shortName(in)) == "ts"

  def readSourceFile(in: String): String = {
    val code = readFile(in)
    val terminatedCode = if (code.lastOption.contains('\n')) code else code + "\n"
    terminatedCode.replace("\r\n", "\n") // if there are any CRLF, replace them with LF only (might happen on Windows)
  }
  case class Item(originalCode: String, included: Boolean, fullName: String) {
    assert(!fullName.contains("../")) // must not contain .., because resolveSibling cannot handle it
    override def toString = s"($fullName:$included)"

    def code: String = {
      // carefully crafted export is used as a file begin / end marker
      // exports are AST nodes, therefore they survive most transformation
      // yet they are mostly ignored, and therefore harmfull
      // we just make sure to avoid a name collision by using a "vendor" prefixed identifier
      def mark(t: String) = s"export const ScalaFromJS_$t='$fullName';\n"
      mark("begin") + originalCode + mark("end")
    }
  }

  def loadControlFile(in: String): ConvertProject = {

    // parse only the control file to read the preprocess rules
    val code = readSourceFile(in)
    val typescript = detectTypescript(in)
    val ext = NodeExtended(parse(code, typescript)).loadConfig(Some(in)).config

    Time("loadControlFile") {
      // do not preprocess the control file - it makes no sense
      // and it is easy for regex rules to match themselves, which breaks the control file
      val predef = if (typescript) predefinedHeadersTS else predefinedHeadersJS
      val predefinedItems = ListMap(predef.map { case (name, code) =>
        name -> Item(code, false, name)
      }:_*)

      val project = ConvertProject(in, ext, predefinedItems ++ ListMap(in -> Item(code, true, in)))

      project.resolveImportsExports
    }
  }

  def loadConfig(ast: Node.Program, root: Option[String]): (ConvertConfig, Node.Program) = {
    var readConfig = Option.empty[ConvertConfig]

    object GetConfig {
      def unapply(arg: Node.Node) = arg match {
        case VarDecl(`configName`, Some(OObject(props)), _) =>
          Some(props)
        case VarDecl(`configName`, x, _) =>
          println(s"Warning: config detected, but not matched ($x)")
          None
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

    val astWithoutConfig = readConfig.fold(ast) { rc =>
      ast.transformAfter { (node, _) =>
        node match {
          case GetConfig(_) =>
            Node.EmptyStatement()
          case _ =>
            node
        }

      }
    }

    val config = readConfig.getOrElse(ConvertConfig())
    root.map(r => config.copy(root = r)).getOrElse(config) -> astWithoutConfig
  }


}


case class ConvertProject(root: String, config: ConvertConfig, items: Map[String, Item]) {
  val values = items.values.toIndexedSeq
  lazy val code = values.map(_.code).mkString
  // offset boundaries for all items, including before the first one (zero), and after the last one (total input lenght)
  lazy val offsets = values.scanLeft(0)((offset, file) => offset + file.code.length)

  def checkIntegrity = {
    val missingEol = values.filterNot(_.code.endsWith("\n"))
    missingEol.map(_.fullName).foreach(s => println(s"Missing eol in $s"))
    val wrongPath = values.filter(i => i.fullName contains "../")
    wrongPath.map(_.fullName).foreach(s => println(s"Wrong path in $s"))
    missingEol.isEmpty && wrongPath.isEmpty
  }

  assert(checkIntegrity)

  def indexOfItem(offset: Int): Int = offsets.segmentLength(_ <= offset) - 1
  def rangeOfPath(path: String): (Int, Int) = {
    val index = values.indexWhere(_.fullName == path)
    (offsets(index), offsets(index + 1))
  }

  def pathForOffset(offset: Int): String = {
    val index = indexOfItem(offset)
    if (values.isDefinedAt(index)) values(index).fullName
    else ""
  }

  @scala.annotation.tailrec
  final def resolveImportsExports: ConvertProject = {
    def readFileAsJs(path: String): (String, String) = {
      val aliasedCode = config.handleAliasPreprocess(path)(readSourceFile(path))
      val code = config.preprocess(path, aliasedCode)
      // try parsing, if unable, return a comment file instead
      try {
        parse(code, detectTypescript(path))
        //println(s"Input file $path")
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
            val wrap = ScriptExtractor.wrapAsJS(simpleName, code)
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
        try {
          readFileAsJs(name)
        } catch {
          case _: Exception if !name.endsWith(extension) =>
            readFileAsJs(name + extension)
        }
      } { item =>
        item.code -> item.fullName
      }
    }

    def readJsFromHtmlFile(namePar: String): Option[(String, String)] = {
      val name = namePar.replace('\\', '/')
      val html = readSourceFile(name)

      ScriptExtractor.fromHTML(name, html).map(js => config.preprocess(name, config.handleAliasPreprocess(name)(js)) -> name)
    }

    val ast = try {
      //println("** Parse\n" + items.mkString("\n"))
      parse(code, detectTypescript(root))
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
    val notFound = mutable.Set.empty[String]
    def checkForComment(node: Node.Node, comment: String): Boolean = {
      Option(node.leadingComments).toSeq.flatten.exists { commentToken =>
        commentToken.value contains comment
      }
    }

    val rootExtension = PathUtils.shortName(root).dropWhile(_ != '.')
    val defaultExtension = if (rootExtension.nonEmpty) rootExtension else ".js"

    def alreadyPresent(target: Seq[(String, String)], path: String) = {
      target.exists(i => i._2 == path || i._2 == path + defaultExtension)
    }

    def reportParseError(ex: Exception, path: String, importPath: String, inFile: String) = {
      if (FileAccess.matchFileNotFound(ex)) {
        // print a message, but try to continue
        if (!notFound.contains(path)) {
          println(s"warning: file $path ($importPath) not found (from $inFile)")
          notFound += path
        }
      } else {
        // print a message, but try to continue
        if (!notFound.contains(path)) {
          println(s"warning: file $path ($importPath) not parsed (from $inFile) $ex")
          notFound += path
        }
      }
    }

    ast.walk {
      case i: Node.ImportDeclaration =>
        val example = checkForComment(i, "@example")
        val target = if (example) exampleBuffer else includeBuffer
        val inFile = pathForOffset(i.range._1)
        val importPath = i.source.value
        val path = resolveSibling(inFile, importPath)
        try {
          if (!alreadyPresent(target, path)) {
            target append readJsFile(path)
          }
        } catch {
          case ex: Exception =>
            reportParseError(ex, path, importPath, inFile)
        }
        false
      case e@ExportFromSource(Defined(StringLiteral(source))) =>
        // ignore exports in imported files
        e.start.foreach { s =>
          val index = indexOfItem(s)
          //println(s"index of ${s.pos} = $index in $offsets")
          if (values.isDefinedAt(index) && values(index).included) {
            val inFile = pathForOffset(s)
            val path = resolveSibling(inFile, source)
            if (!alreadyPresent(includeBuffer, path)) {
              val htmlScript = checkForComment(e, "@html-script")
              try {
                if (htmlScript) {
                  // TODO: support wildcards for other file types as well
                  val htmlRelativeWildcard = "/*.html"
                  if (path.endsWith(htmlRelativeWildcard)) {
                    // TODO: allow exclusion
                    val allHtmlFiles: Seq[String] = FileAccess.listFiles(path.dropRight(htmlRelativeWildcard.length))
                    for (f <- allHtmlFiles if f.endsWith(".html")) {
                      includeBuffer appendAll readJsFromHtmlFile(f)
                    }
                  } else {
                    includeBuffer appendAll readJsFromHtmlFile(path)
                  }
                }
                else {
                  includeBuffer append readJsFile(path)
                }
              } catch {
                case ex: Exception =>
                  reportParseError(ex, path, source, inFile)
              }
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

      val old = items.view.mapValues(i => i.copy(included = i.included || markAsIncludes.exists(_._2 == i.fullName))).toMap
      val examples = toExample.map(nb => mapFile(nb, false))
      val includes = toInclude.map(nb => mapFile(nb, true))

      assert((old.keySet intersect examples.map(_._1).toSet).isEmpty)
      assert((old.keySet intersect includes.map(_._1).toSet).isEmpty)
      //println(s"  old ${old.map(_.name).mkString(",")}")

      //println("Inputs read")
      ConvertProject(root, config, old ++ examples ++ includes).resolveImportsExports
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
          parse(code, detectTypescript(name))
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
      parse(compositeFile, detectTypescript(root))
    }

    val ext = NodeExtended(ast).loadConfig(Some(root))

    val parts = (fileOffsets lazyZip fileOffsets.drop(1) lazyZip exports).map { case (from, to, item) =>
      val filePath = relativePath(root, item.fullName)
      ScalaOut.Part(from, to, name = filePath)
    }

    val astOptimized = if (true) Transform(ext) else ext

    val outConfig = ScalaOut.Config(
      parts = parts,
      root = root,
      cfg = ext.config
    )
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
