package com.github.opengrabeso.scalafromjs

import scala.util.Try
import PathUtils._
import ConvertProject.{AliasPackageRule, FileEnvironment}
import FileAccess._

import scala.collection.Seq

object CommandLine {

  // return filenames of the output files
  def convertFileToFile(in: String, out: String): Seq[String] = {
    val log = false
    if (log) println(s"Convert $in to $out")

    val project = ConvertProject.loadControlFile(in, FileEnvironment.empty)

    if (log) println(s"Convert ${(project.values.map(_.fullName) zip project.offsets).mkString("\n")}")

    val converted = project.convert

    for ( (inFile, outCode) <- converted.files) yield {

      val inRelative = relativePath(in, inFile)

      val outFileBase = resolveSibling(out, inRelative)

      val outFileCombined = changeExtension(outFileBase, out)

      if (log) println(s"out: $out, in: $in, inFile: $inFile -> $outFileCombined")

      val shortFileName = shortName(inFile)

      val processed = converted.config.postprocess(inFile, outCode)

      val (aliasedName, wrappedOutCode) = converted.config.handleAlias(inRelative)(processed)

      val inFilePackage = aliasedName.split('/').filterNot(_.isEmpty)


      val packageDirectives = inFilePackage.map(item => s"package $item").toSeq
      val packagePrefix = packageDirectives.mkString("", "\n", "\n")

      //println(s"shortName $shortFileName inRelative $inRelative inRelativePath $inRelativePath aliasedName $aliasedName packageDirectives $packageDirectives")

      val extendedPrefix = s"/*\n${Convert.fingerprint}\n$shortFileName\n*/\n\n"
      val outCodeWithPackage = packagePrefix + wrappedOutCode

      //println(s"Write $outFileCombined from $inFile (out: $out)")
      mkAllDirs(outFileCombined)
      writeFile(outFileCombined, extendedPrefix + outCodeWithPackage)
      outFileCombined
    }
  }

  def main(argv: Array[String]): Unit = {
    if (argv.isEmpty) {
      ScalaFromJS.main(argv)
    } else {
      println(s"  args ${argv.mkString(",")}")
      println(s"  fingerprint ${Convert.fingerprint}")
      Time("  conversion", true) {
        convertFileToFile(argv(0), argv(1))
      }
    }
  }
}
