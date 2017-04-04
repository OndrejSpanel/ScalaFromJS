package com.github.opengrabeso

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@JSGlobal("$require")
@js.native
object Require extends js.Any

object CommandLine {
  def getRequire: js.Dynamic = {
    // workaround:
    // js.Dynamic.global.require is undefined when running using "runa" sbt task (node.exe index.js)
    // test and run work fine
    // we do not need require when running in a browser (no file access)
    val r = js.Dynamic.global.require
    if (js.isUndefined(r)) {
      Require.asInstanceOf[js.Dynamic]
    } else r
  }

  lazy val require = getRequire

  //println(s"require $require")
  //println(s"global ${js.Dynamic.global}")

  lazy val fs = require("fs")
  lazy val os = require("os")
  lazy val process = require("process")
  lazy val path = require("path")

  // TODO: facade instead of Dynamic

  def readFile(name: String): String = {
    fs.readFileSync(name).toString
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

  object ErrorCode {
    def unapply(arg: Any) = {
      val argDyn = arg.asInstanceOf[js.Dynamic]
      argDyn.code match {
        case c if js.isUndefined(c) => None
        case c => Some(c.toString)

      }
      Some(argDyn.code.asInstanceOf[String])
    }
  }
  def mkdir(path: String): Unit = {
    //println(s"mkdir $path")
    try {
      fs.mkdirSync(path)
    } catch {
      case ex@js.JavaScriptException(ErrorCode("EPERM")) =>
        // check if it already exists
        try {
          // if the directory exists, do not care about permission failure while creating it
          val stat = fs.statSync(path)
          if (!stat.isDirectory().asInstanceOf[Boolean]) {
            throw ex
          }
        } catch {
          case js.JavaScriptException(x) =>
            // when stat has failed, throw the original exception
            throw ex
        }

      case js.JavaScriptException(ErrorCode("EEXIST"))  =>
        // if exists, everything is OK
    }
  }
  /*
  * create all parent directories
  * @param path to the file we want to create
  * */

  def mkAllDirs(path: String) = {
    val subpaths = path.split("/", -1)
    //println(subpaths.mkString(" - "))
    subpaths.tail.foldLeft(subpaths.head) { (folder, elem) =>
      mkdir(folder)
      folder + "/" + elem
    }

  }

  lazy val argv: Seq[String] = {
    process.argv.asInstanceOf[js.Array[String]]
  }


  def resolveSibling(path: String, short: String): String = {
    val dir = path.lastIndexOf('/')
    if (dir < 0) short
    else {
      val currentPrefix = "./"
      val parentPrefix ="../"
      if (short.startsWith(parentPrefix)) {
        resolveSibling(path.take(dir), short.drop(parentPrefix.length))
      } else {
        val shortFixed = if (short.startsWith(currentPrefix)) short.drop(currentPrefix.length) else short
        path.take(dir + 1) + shortFixed
      }
    }
  }

  def relativePath(base: String, path: String): String = {
    val dir = base.lastIndexOf('/')
    if (dir < 0) path
    else {
      val baseDir = base.take(dir + 1)
      if (path.startsWith(baseDir)) path.drop(baseDir.length)
      else path
    }

  }

  def shortName(path: String): String = {
    val dir = path.lastIndexOf('/')
    if (dir < 0) path
    else path.drop(dir + 1)
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

    val project = ConvertProject.loadControlFile(in)

    val converted = project.convert

    for ( (inFile, outCode) <- converted) yield {

      val inRelative = relativePath(in, inFile)

      val outFileBase = resolveSibling(out, inRelative)

      val outFileCombined = changeExtension(outFileBase, out)

      //println(s"out: $out, in: $in, inFile: $inFile -> $outFileCombined")

      val extendedPrefix = s"/*\n${ScalaFromJS.fingerprint()}\n${shortName(inFile)}\n*/\n\n"
      //println(s"Write $outFileCombined from $inFile (out: $out)")
      mkAllDirs(outFileCombined)
      writeFile(outFileCombined, extendedPrefix + outCode)
      outFileCombined
    }
  }

  def apply() = {
    val realArgs = argv.drop(2)
    println(s"  args ${realArgs.mkString(",")}")

    if (realArgs.length == 2) {
      convertFileToFile(realArgs(0), realArgs(1))
    } else {
      convertFileToFile("temp/test.js", "temp/out/test.scala")
    }

  }
}
