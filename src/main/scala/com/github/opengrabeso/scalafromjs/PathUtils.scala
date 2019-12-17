package com.github.opengrabeso.scalafromjs

object PathUtils {
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


  // inspired by https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#resolveSibling(java.nio.file.Path)
  @scala.annotation.tailrec
  def resolveSibling(path: String, short: String): String = {
    assert(!path.contains("../"))
    val dir = path.lastIndexOf('/')
    if (dir < 0) short
    else {
      val currentPrefix = "./"
      val parentPrefix ="../"
      if (short.startsWith(parentPrefix)) {
        resolveSibling(path.take(dir), short.drop(parentPrefix.length))
      } else {
        if (short.startsWith(currentPrefix)) {
          resolveSibling(path, short.drop(currentPrefix.length))
        } else {
          assert(!short.contains("../"))
          path.take(dir + 1) + short
        }
      }
    }
  }

  // make path relative to a base file. When a folder is given as base, it must include terminating "/"
  def relativePath(base: String, path: String): String = {
    val dir = base.lastIndexOf('/')
    if (dir < 0) path
    else {
      val baseDir = base.take(dir + 1)
      if (path.startsWith(baseDir)) path.drop(baseDir.length)
      else path
    }

  }

  def terminatedPath(s: String): String = {
    val pathEnd = "/"
    if (s.endsWith(pathEnd)) s else s + pathEnd
  }


}
