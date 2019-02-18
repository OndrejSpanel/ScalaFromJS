package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._

object Convert {
  lazy val fingerprint = {
    // https://stackoverflow.com/a/13546822/16673
    val p = getClass.getPackage
    val name = Option(p.getImplementationTitle).getOrElse("ScalaFromJS")
    val version = Option(p.getImplementationVersion).getOrElse("Dev")
    val timestamp = {

      import java.text.SimpleDateFormat
      import java.util.TimeZone
      val date = new java.util.Date()
      val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

      // Use Madrid's time zone to format the date in
      df.setTimeZone(TimeZone.getDefault)

      df.format(date)
    }

    // Uglify Scala.js version was named like this: ScalaFromJS: 2017-11-22 17:48:10.567
    name + ": " + version + " " + timestamp
  }



  def prefix(header: Boolean) = if (header) s"/* $fingerprint*/\n\n" else ""

  def apply(code: String, header: Boolean = true): String = {
    // split input file based on //file: comments

    val files = code.split("\\/\\/file\\:")
    if (files.lengthCompare(1) <= 0) {

      // note: here we parse only to load the preprocess config
      val preparse = parse(code)
      val cfg = NodeExtended(preparse).loadConfig.config

      val preprocessed = cfg.preprocess(code)

      val ast = parse(preprocessed)

      val ext = NodeExtended(ast).loadConfig

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
