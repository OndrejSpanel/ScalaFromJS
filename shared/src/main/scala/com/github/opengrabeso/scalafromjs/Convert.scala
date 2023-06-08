package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima.{GlobalConfig => _, _}
import com.github.opengrabeso.scalafromjs.ConvertProject.{ConvertConfig, FileEnvironment}

object Convert {

  def fingerprint: String = GlobalConfig.fingerprint

  def prefix(header: Boolean) = if (header) s"/* $fingerprint*/\n\n" else ""

  def apply(code: String, header: Boolean = true, typescript: Boolean = false): String = {
    // split input file based on //file: comments

    val files = code.split("\\/\\/file\\:")
    if (files.lengthCompare(1) <= 0) {

      val script = ScriptExtractor(code)
      // note: here we parse only to load the preprocess config
      val preparse = parse(script, typescript)
      val cfg = NodeExtended(preparse).loadConfig().config

      val preprocessed = cfg.preprocess("", script)

      val ast = parse(preprocessed, typescript)

      val ext = NodeExtended(ast).loadConfig()

      val astOptimized = Transform(ext)
      val outConfig = ScalaOut.Config(cfg = cfg)
      val ret = prefix(header) + ScalaOut.output(astOptimized, script, outConfig).mkString

      ext.config.postprocess("", ret)
    } else {
      def isEmptyFile(s: String) = {
        s.count(_.isWhitespace) == s.length
      }
      val allFileParts = files.toSeq.filterNot(isEmptyFile).map { file =>
        val (fileName, fileContent) = file.span(!_.isWhitespace)

        val terminatedCode = if (fileContent.lastOption.contains('\n')) fileContent else fileContent + "\n"

        val included = !fileName.endsWith(".d.ts")
        // no control file, just a composite file
        fileName -> ConvertProject.Item(terminatedCode, included, fileName)

      }
      // do not include d.ts files, they will be loaded by a rule if necessary
      val (fileParts, dtsFiles) = allFileParts.partition(_._2.included)
      val fs = FileEnvironment(dtsFiles.map(x => x._1 -> x._2.originalCode).toMap)
      val config = ConvertConfig(fs = fs)
      val convertResult = ConvertProject("", config, fileParts.toMap).convert

      val converted = convertResult.files.map { case (name, content) =>
        s"\n//file:$name\n\n" + convertResult.config.postprocess(name, content)
      }.mkString
      prefix(header) + converted
    }
  }

  def project(in: String, header: Boolean = true): String = {
    val project = ConvertProject.loadControlFile(in, FileEnvironment.empty)
    val converted = project.convert.files.map(_._2).mkString
    prefix(header) + converted
  }


}
