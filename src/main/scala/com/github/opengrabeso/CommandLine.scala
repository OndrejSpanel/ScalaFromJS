package com.github.opengrabeso

import Uglify._
import UglifyExt._

import scala.scalajs.js
import scala.collection.mutable
import scala.scalajs.js.annotation.JSImport
import scala.util.{Failure, Success, Try}

@JSImport("fs", JSImport.Namespace)
@js.native
object FS extends js.Object {
  def readFileSync(name: String): js.Dynamic = js.native

  def unlinkSync(name: String): Unit = js.native
}

@JSImport("process", JSImport.Namespace)
@js.native
object Process extends js.Object

@JSImport("path", JSImport.Namespace)
@js.native
object Path extends js.Object

@JSImport("os", JSImport.Namespace)
@js.native
object OS extends js.Object

object CommandLine {
  // TODO: facade instead of Dynamic
  val fs = FS.asInstanceOf[js.Dynamic]
  val process = Process.asInstanceOf[js.Dynamic]
  val path = Path.asInstanceOf[js.Dynamic]
  val os = OS.asInstanceOf[js.Dynamic]


  def readFile(name: String): String = {
    FS.readFileSync(name).toString
  }

  def removeFile(name: String): Unit = {
    //println(s"Remove file $name")
    fs.unlinkSync(name)
  }

  def writeFile(name: String, content: String): Unit = {
    fs.writeFileSync(name, content)
  }

  lazy val separator = path.sep.asInstanceOf[String]

  def terminateBySeparator(path: String): String = {
    if (path endsWith separator) path else path + separator
  }

  // replace separator with a slash
  def separatorAsSlash(path: String) = path.replace(separator, "/")

  // replace slash with a platform specific separator
  def separatorAsPlatform(path: String) = path.replace("/", separator)

  def withTempDir(prefix: String)(perform: String => Unit): Unit = {
    val dir = os.tmpdir().asInstanceOf[String]
    val tempBase = terminateBySeparator(dir)
    val tempDir = fs.mkdtempSync(tempBase + prefix).asInstanceOf[String]
    val dirName = separatorAsSlash(terminateBySeparator(tempDir))
    try {
      perform(dirName)
    } finally {
      try {
        rmdir(dirName)
      } catch {
        case scala.util.control.NonFatal(_) =>
      }
    }
  }

  def rmdir(path: String) = {
    // safety check: never ever delete a root path
    if (path.isEmpty || path.count(_ == '/') < 2) {
      throw new UnsupportedOperationException("Refused to delete '$path'")
    }
    //println(s"rmdir $path")
    fs.rmdirSync(path)
  }

  lazy val argv: Seq[String] = {
    process.argv.asInstanceOf[js.Array[String]]
  }

  case class ConvertProject(imports: Seq[String], exports: Seq[String])

  def loadControlFile(ast: AST_Toplevel): Try[ConvertProject] = {
    // check export / import statements
    val importBuffer = new mutable.ArrayBuffer[String]
    val exportBuffer = new mutable.ArrayBuffer[String]
    ast.walk {
      case i: AST_Import =>
        importBuffer append i.module_name.value
        false
      case e: AST_Export =>
        e.module_name.foreach { name =>
          exportBuffer append name.value
        }
        false
      case _ =>
        false

    }
    if (exportBuffer.nonEmpty) {
      Success(ConvertProject(importBuffer, exportBuffer))
    } else {
      Failure(new NoSuchElementException)
    }
  }

  def resolveSibling(path: String, short: String): String = {
    val dir = path.lastIndexOf('/')
    if (dir < 0) short
    else path.take(dir + 1) + short
  }

  def shortName(path: String): String = {
    val dir = path.lastIndexOf('/')
    if (dir < 0) path
    path.drop(dir + 1)
  }

  def extension(path: String): String = {
    val short = shortName(path)
    val ext = short.lastIndexOf('.')
    if (ext<0) ""
    else short.drop(ext + 1)
  }
  def changeExtension(path: String, pathWithExtension: String): String = {
    val ext = extension(path)
    path.dropRight(ext.length) + extension(pathWithExtension)
  }

  // return filenames of the output files
  def convertFileToFile(in: String, out: String): Seq[String] = {
    //println(s"Convert $in to $out")
    val code = readFile(in)

    val ast = parse(code, defaultUglifyOptions.parse)

    val controlFile = loadControlFile(ast)
    controlFile.toOption.fold{
      val astOptimized = Transform(ast)
      val output = s"/* ${ScalaFromJS.fingerprint()}*/\n\n" + ScalaOut.output(astOptimized, code).mkString
      writeFile(out, output)
      Seq(out)
    }{ project =>
      def loadFiles(names: Seq[String]) = names.map { filename =>
        val singlePath = resolveSibling(in, filename)
        readFile(singlePath.toString)
      }


      val exportsFiles = loadFiles(project.exports)
      val importsFiles = loadFiles(project.imports)


      val fileOffsets = exportsFiles.scanLeft(0)((offset, file) => offset + file.length)
      //println(fileOffsets.drop(1) zip project.exports)

      val compositeFile = (exportsFiles ++ importsFiles).mkString

      val ast = parse(compositeFile, defaultUglifyOptions.parse)

      val astOptimized = Transform(ast)
      val outConfig = ScalaOut.Config().withParts(fileOffsets drop 1)
      val output = ScalaOut.output(astOptimized, code, outConfig)

      for ( (outCode, inFile) <- output zip project.exports) yield {

        val outFileBase = resolveSibling(out, inFile)

        val outFileCombined = changeExtension(outFileBase, out)

        val extendedPrefix = s"/*\n${ScalaFromJS.fingerprint()}\n${shortName(inFile)}\n*/\n\n"
        //println(s"Write $outFileCombined from $inFile (out: $out)")
        writeFile(outFileCombined, extendedPrefix + outCode)
        outFileCombined
      }
    }
  }

  def apply() = {
    val realArgs = argv.drop(2)
    println(s"  args ${realArgs.mkString(",")}")

    convertFileToFile("temp/input.js", "temp/output.scala")

  }
}
