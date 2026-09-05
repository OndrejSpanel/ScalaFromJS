package com.github.opengrabeso.scalafromjs

import scala.collection.Seq

object FileAccess {
  def readFile(path: String): String = {
    throw new NotImplementedError()
  }
  def writeFile(path: String, content: String): Unit = {
    throw new NotImplementedError()
  }

  def mkAllDirs(path: String): Unit = throw new NotImplementedError()


  def matchFileNotFound(ex: Exception): Boolean = false

  def listFiles(path: String): Seq[String] = throw new NotImplementedError()

  def removeStaleGeneratedFiles(root: String, retained: Seq[String]): Seq[String] = throw new NotImplementedError()


}
