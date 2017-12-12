package com.github.opengrabeso

import scala.util.Try
import PathUtils._
import ConvertProject.AliasPackageRule

object CommandLine {

  def readFile(path: String): String = {
    val source = scala.io.Source.fromFile("file.txt")
    val lines = try source.mkString finally source.close()
    lines
  }
  def writeFile(path: String, content: String): Unit = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets

    Files.write(Paths.get(path), content.getBytes(StandardCharsets.UTF_8))
  }

  def mkAllDirs(path: String): Unit = {
    val dir = new java.io.File(path)
    dir.mkdirs()

  }

  // return filenames of the output files
  def convertFileToFile(in: String, out: String): Seq[String] = {
    val log = false
    if (log) println(s"Convert $in to $out")

    val project = ConvertProject.loadControlFile(in)

    if (log) println(s"Convert ${(project.values.map(_.fullName) zip project.offsets).mkString("\n")}")

    val converted = project.convert

    for ( (inFile, outCode) <- converted.files) yield {

      val inRelative = relativePath(in, inFile)

      val outFileBase = resolveSibling(out, inRelative)

      val outFileCombined = changeExtension(outFileBase, out)

      if (log) println(s"out: $out, in: $in, inFile: $inFile -> $outFileCombined")

      val inRelativePathIndex = inRelative.lastIndexOf('/')
      val inRelativePath = if (inRelativePathIndex < 0) "" else inRelative.take(inRelativePathIndex)
      val shortFileName = shortName(inFile)

      def handleAlias(filePath: String, content: String): (String, String) = {
        // check if we match any alias key
        val terminated = terminatedPath(inRelativePath)
        for (alias <- converted.config.collectRules[AliasPackageRule]) {
          val named = alias.namePackage(terminated)
          if (named.isDefined) {
            return (named.get, alias.applyTemplate(shortFileName, content))
          }
        }
        (filePath, content)
      }

      val processed = converted.config.postprocess(outCode)

      val (aliasedName, wrappedOutCode) = handleAlias(inRelative, processed)

      val inFilePackage = aliasedName.split('/')


      val packageDirectives = inFilePackage.map(item => s"package $item").toSeq
      val packagePrefix = packageDirectives.mkString("", "\n", "\n")

      //println(s"shortName $shortFileName inRelative $inRelative inRelativePath $inRelativePath aliasedName $aliasedName packageDirectives $packageDirectives")

      val extendedPrefix = s"/*\n${ScalaFromJS.fingerprint}\n$shortFileName\n*/\n\n"
      val outCodeWithPackage = packagePrefix + wrappedOutCode

      val skip = Try {
        val existingFile = readFile(outFileCombined)
        val existingLines = scala.io.Source.fromString(existingFile).getLines().toSeq
        val outputLines = scala.io.Source.fromString(outCodeWithPackage).getLines().toSeq
        existingLines match {
          case "/*" +: _ +:  _ +:  "*/" +:  "" +: `outputLines` =>
            // skip storing the file if the only difference between the file and the existing version is the prefix
            //println(s"  Identical content for $outFileBase")
            true
          /*
          case "/*" +: _ +:  _ +:  "*/" +:  "" +: rest  =>
            println(s"  prefix detected for outFileBase, rest starts with ${rest.head}")
            false
          */
          case _ =>
            false
        }
      }.getOrElse(false)
      // check existing prefix
      // error handling - non-existing file?
      if (!skip) {
        //println(s"Write $outFileCombined from $inFile (out: $out)")
        mkAllDirs(outFileCombined)
        writeFile(outFileCombined, extendedPrefix + outCodeWithPackage)
      }
      outFileCombined
    }
  }

  def apply(argv: String*) = {
    val realArgs = argv.drop(1)
    println(s"  args ${argv.mkString(",")}")

    if (realArgs.length == 2) {
      convertFileToFile(realArgs(0), realArgs(1))
    } else {
      convertFileToFile("temp/test.js", "temp/out/test.scala")
    }

  }
}
