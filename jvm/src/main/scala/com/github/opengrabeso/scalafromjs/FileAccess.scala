package com.github.opengrabeso.scalafromjs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.Seq
import scala.jdk.CollectionConverters._

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

  /**
   * Remove outputs left by an earlier conversion which are not part of the
   * current project. Files without the ScalaFromJS header are never touched.
   */
  def removeStaleGeneratedFiles(root: String, retained: Seq[String]): Seq[String] = {
    val rootPath = Paths.get(root)
    if (!Files.exists(rootPath)) return Seq.empty

    val retainedPaths = retained.iterator
      .map(path => Paths.get(path).toAbsolutePath.normalize())
      .toSet
    val files = Files.walk(rootPath)
    try {
      files.iterator().asScala
        .filter(Files.isRegularFile(_))
        .filter(_.getFileName.toString.endsWith(".scala"))
        .filterNot(path => retainedPaths.contains(path.toAbsolutePath.normalize()))
        .filter(isScalaFromJSOutput)
        .map { path =>
          Files.delete(path)
          path.toString
        }
        .toSeq
    } finally {
      files.close()
    }
  }

  private def isScalaFromJSOutput(path: java.nio.file.Path): Boolean = {
    val reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)
    try {
      reader.readLine() == "/*" && Option(reader.readLine()).exists(_.startsWith("ScalaFromJS:"))
    } finally {
      reader.close()
    }
  }

}
