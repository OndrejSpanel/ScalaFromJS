package com.github.opengrabeso

import com.github.opengrabeso.esprima._
import _root_.esprima._

object Convert {
  def prefix(header: Boolean) = if (header) s"/* ${ScalaFromJS.fingerprint}*/\n\n" else ""

  def apply(code: String, header: Boolean = true): String = {
    // split input file based on //file: comments

    val files = code.split("\\/\\/file\\:")
    if (files.lengthCompare(1) <= 0) {

      val res = minify(code, defaultUglifyOptions)
      val ast = res.top

      val ext = Transform.NodeExtended(ast).loadConfig

      val astOptimized = Transform(ext)
      val ret = prefix(header) + ScalaOut.output(astOptimized, code).mkString

      ext.config.postprocess(ret)
    } else {
      val fileParts = files.toSeq.filterNot(_.isEmpty).map { file =>
        val (fileName, fileContent) = file.span(!_.isWhitespace)

        val terminatedCode = if (fileContent.lastOption.contains('\n')) fileContent else fileContent + "\n"

        // no control file, just a composite file
        fileName -> ConvertProject.Item(terminatedCode, true, fileName)

      }
      val convertResult = ConvertProject("", fileParts.toMap).convert

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
