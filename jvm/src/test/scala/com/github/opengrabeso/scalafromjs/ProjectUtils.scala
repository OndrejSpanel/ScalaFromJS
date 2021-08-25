package com.github.opengrabeso.scalafromjs

import java.nio.file.{Files, Paths}
import PathUtils._

import scala.jdk.CollectionConverters._
import scala.collection.Seq


trait ProjectUtils extends TestUtils {
  import CommandLine._
  import FileAccess._

  def withTempDir[T](path: String)(f: String => T): T = {
    val dir = Files.createTempDirectory(path)
    val pathSlash = terminatedPath(dir.toString.replace('\\', '/'))
    try {
      f(pathSlash)
    } finally {
      try {
        val path = Paths.get(pathSlash)
        Files.walk(path).iterator.asScala.toSeq.reverse.foreach(f => Files.delete(f))
      } catch {
        case _: Exception =>
      }
    }
  }

  def forEachFileWithCleanup(files: Seq[String])(f: String => Unit): Unit = {
    // no need to clean up, it will be done in withTempDir
    files.foreach(f)
  }

  def convertProject(controlFile: String): String = {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile(rscPath(controlFile), temp + "xxx.scala")
      val sb = new StringBuilder
      forEachFileWithCleanup(out) { f =>
        // for each file verify the resulting file is a Scala file with a comment
        assert(f.endsWith(".scala"))
        val outCode = readFile(f)
        sb append outCode
        exec check ResultCheck(outCode).required("/*", "*/")
      }
      sb.result()
    }
  }


}
