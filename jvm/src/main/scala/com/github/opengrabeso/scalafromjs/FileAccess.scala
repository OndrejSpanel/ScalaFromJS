package com.github.opengrabeso.scalafromjs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object FileAccess {
  def readFile(path: String): String = {
    val source = scala.io.Source.fromFile(path)(scala.io.Codec.UTF8)
    val lines = try source.mkString finally source.close()
    lines
  }
  def writeFile(path: String, content: String): Unit = {
    Files.write(Paths.get(path), content.getBytes(StandardCharsets.UTF_8))
  }

  def mkAllDirs(path: String): Unit = {
    val dir = new java.io.File(path)
    dir.getParentFile.mkdirs()

  }

  def matchFileNotFound(ex: Exception): Boolean = ex.isInstanceOf[java.io.FileNotFoundException]

  def listFiles(path: String): Seq[String] = {
    val dir = new java.io.File(path)
    dir.list().map(dir.toPath.resolve(_).toString)
  }

}
