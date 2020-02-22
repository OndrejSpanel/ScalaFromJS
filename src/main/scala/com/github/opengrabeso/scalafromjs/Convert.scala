package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._

object Convert {

  def fingerprint: String = GlobalConfig.fingerprint

  def prefix(header: Boolean) = if (header) s"/* $fingerprint*/\n\n" else ""

  def apply(code: String, header: Boolean = true, typescript: Boolean = false): String = {
    // split input file based on //file: comments

    val files = code.split("\\/\\/file\\:")
    if (files.lengthCompare(1) <= 0) {

      // note: here we parse only to load the preprocess config
      val preparse = parse(code, typescript)
      val cfg = NodeExtended(preparse).loadConfig().config

      val preprocessed = cfg.preprocess(code)

      val ast = parse(preprocessed, typescript)

      val ext = NodeExtended(ast).loadConfig()

      val astOptimized = Transform(ext)
      val ret = prefix(header) + ScalaOut.output(astOptimized, code).mkString

      ext.config.postprocess(ret)
    } else {
      def isEmptyFile(s: String) = {
        s.count(_.isWhitespace) == s.length
      }
      val fileParts = files.toSeq.filterNot(isEmptyFile).map { file =>
        val (fileName, fileContent) = file.span(!_.isWhitespace)

        val terminatedCode = if (fileContent.lastOption.contains('\n')) fileContent else fileContent + "\n"

        // no control file, just a composite file
        fileName -> ConvertProject.Item(terminatedCode, true, fileName)

      }
      val convertResult = ConvertProject("", identity, fileParts.toMap).convert

      val converted = convertResult.files.map { case (name, content) =>
        s"\n//file:$name\n\n" + convertResult.config.postprocess(content)
      }.mkString
      prefix(header) + converted
    }
  }

  def project(in: String, header: Boolean = true): String = {
    val project = ConvertProject.loadControlFile(in)
    val converted = project.convert.files.map(_._2).mkString
    prefix(header) + converted
  }


}
