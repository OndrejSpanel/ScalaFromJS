package com.github.opengrabeso

import scala.util.{Failure, Success, Try}

trait TestUtils {
  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  def normalizeEol(str: String) = str.replace("\r\n", "\n")

  object ConversionCheck {
    val standardForbidden = Seq(";", "/* Unsupported:")

    def apply(code: String, mustHave: String*) = new ConversionCheck(code, mustHave)
  }

  case class ConversionCheck(code: String, mustHave: Seq[String], mustNotHave: Seq[String] = ConversionCheck.standardForbidden) {

    def checkResult(result: String): Try[Unit] = {
      val missing = mustHave.filter(!result.contains(_))
      val forbidden = mustNotHave.filter(result.contains(_))
      if (missing.isEmpty & forbidden.isEmpty) {
        Success(())
      } else Failure {
        def stringList(ss: Seq[String]) = ss.map("  " + _ + "\n").mkString

        val missingStr = if (missing.nonEmpty) "Missing: \n" + stringList(missing) else ""
        val forbiddenStr = if (forbidden.nonEmpty) "Forbidden: \n" + stringList(forbidden) else ""

        new UnsupportedOperationException(missingStr + forbiddenStr + "in \n" + result)
      }
    }

    def produceResult = Main.convert(code)

    val result = produceResult
    // TODO: better error reporting
    checkResult(result).failed.foreach(throw _)
  }

}
