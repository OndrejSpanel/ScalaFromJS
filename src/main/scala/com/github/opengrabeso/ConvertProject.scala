package com.github.opengrabeso

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import CommandLine._
import com.github.opengrabeso.Transform.AST_Extended

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.{JavaScriptException, RegExp}

import ConvertProject._ // import before the object - see http://stackoverflow.com/a/43298181/16673

object ConvertProject {

  def loadStringValue(o: AST_Object, name: String): Option[String] = {
    o.properties.collectFirst {
      case AST_ObjectKeyVal(`name`, AST_String(value)) => value
    }
  }

  trait Rule {
    def apply(c: AST_Extended): AST_Extended
  }

  case class MemberDesc(cls: RegExp, name: RegExp)

  object MemberDesc {
    def load(o: AST_Object): MemberDesc = {
      val cls = loadStringValue(o, "cls").map(RegExp.apply(_))
      val name = loadStringValue(o, "name").map(RegExp.apply(_))
      MemberDesc(cls.get, name.get)
    }
  }

  case class DeleteMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      TransformClasses.deleteMembers(n, member)
    }
  }

  case class IsClassMemberRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      TransformClasses.replaceIsClass(n, member)
    }
  }

  case class MakePropertyRule(member: MemberDesc) extends Rule {
    override def apply(n: AST_Extended) = {
      TransformClasses.makeProperties(n, member)
    }
  }

  val configName = "ScalaFromJS_settings"

  case class ConvertConfig(rules: Seq[Rule] = Seq.empty)

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
                case Some(opName) =>
                  throw new UnsupportedOperationException(s"Unknown operation $opName for member $m")
                case _ =>
                  throw new UnsupportedOperationException(s"Missing operation for member $m")
              }
            case _ =>
              None
          }
        case n =>
          throw new UnsupportedOperationException(s"Unexpected config entry of type ${nodeClassName(n)}")
      }
      //println(rules)
      ConvertConfig(rules)
    }
  }

  case class Item(code: String, included: Boolean, fullName: String) {
    override def toString = s"($fullName:$included)"
  }

  def loadControlFile(in: String): ConvertProject = {
    Time("loadControlFile") {
      val code = readFile(in)
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

  def indexOfItem(offset: Int): Int = offsets.prefixLength(_ <= offset) - 1

  def pathForOffset(offset: Int): String = {
    val index = indexOfItem(offset)
    if (values.isDefinedAt(index)) values(index).fullName
    else ""
  }

  final def resolveImportsExports: ConvertProject = {
    def readFileAsJs(path: String): (String, String) = {
      val code = readFile(path)
      // try parsing, if unable, return a comment file instead
      try {
        parse(code, defaultUglifyOptions.parse)
        code -> path
      } catch {
        case JavaScriptException(ex) if ex.isInstanceOf[JS_Parse_Error] =>
          //println(s"Parse ex: ${ex.toString} in $path")
          //val wrap = "// " + shortName(path) + "\n/*\n" + code + "\n*/\n"
          val short = shortName(path)
          val dot = short.indexOf('.')
          val simpleName = if (dot <0) short else short.take(dot)
          // TODO: embed wrapped code as a variable (note: JS syntax is needed)
          val wrap = s"var $simpleName = " + "\"\"\n"
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
      parse(code, defaultUglifyOptions.parse)
    } catch {
      case ex@JavaScriptException(err: JS_Parse_Error) =>
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

  def convert: Seq[(String, String)] = {
    val exportsImports = values.sortBy(!_.included)
    //println(s"exportsImports ${exportsImports.map(_.copy(code = ""))}")

    if (false) { // debugging the parse - parse files one by one to pinpoint a problem location
      for (ConvertProject.Item(code, _, name) <- exportsImports) {
        try {
          println(s"Parse $name")
          parse(code, defaultUglifyOptions.parse)
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
      parse(compositeFile, defaultUglifyOptions.parse)
    }

    val ext = AST_Extended(ast).loadConfig

    val astOptimized = if (true) Transform(ext) else ext

    val outConfig = ScalaOut.Config().withParts(fileOffsets drop 1).withRoot(root)
    //println(s"$outConfig")
    val output = ScalaOut.output(astOptimized, compositeFile, outConfig)

    for ( (outCode, ConvertProject.Item(_, _, inFile)) <- output zip exports) yield {
      inFile -> outCode
    }
  }
}
