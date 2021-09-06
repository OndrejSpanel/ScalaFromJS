package com.github.opengrabeso.scalafromjs

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


}
