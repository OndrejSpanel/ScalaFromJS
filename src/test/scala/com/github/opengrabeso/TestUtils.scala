package com.github.opengrabeso

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait TestUtils {
  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  def normalizeEol(str: String) = str.replace("\r\n", "\n")

  object ConversionCheck {
    val standardForbidden = Seq(";", "/* Unsupported:")
  }

  case class ConversionCheck(code: String, mustHave: Seq[String] = Seq(), mustNotHave: Seq[String] = ConversionCheck.standardForbidden) {
    def requiredNothing = copy(mustHave = Seq())
    def forbiddenNothing = copy(mustNotHave = Seq())

    def required(add: String*) = copy(mustHave = mustHave ++ add)
    def forbidden(add: String*) = copy(mustNotHave = mustNotHave ++ add)

    def checkResult(result: String): Try[Unit] = {
      val missing = mustHave.filter(s => !result.contains(normalizeEol(s)))
      val forbidden = mustNotHave.filter(s => result.contains(normalizeEol(s)))
      if (missing.isEmpty & forbidden.isEmpty) {
        Success(())
      } else Failure {
        def stringList(ss: Seq[String]) = ss.map("  " + _ + "\n").mkString

        val missingStr = if (missing.nonEmpty) "Missing: \n" + stringList(missing) else ""
        val forbiddenStr = if (forbidden.nonEmpty) "Forbidden: \n" + stringList(forbidden) else ""

        new UnsupportedOperationException(missingStr + forbiddenStr + "in \n" + result)
      }
    }

    val convert = Convert(code)

    def execute() = {
      checkResult(convert).failed.foreach(throw _)
    }

    //noinspection UnitMethodIsParameterless
    def unary_~ = execute()
  }

  object execute {
    def check(setup: ConversionCheck) = setup.execute()
  }

  def rsc(path: String) = {
    import scalajs.js.Dynamic.{global => g}
    val fs = g.require("fs")
    val process = g.require("process")

    // TODO: facade instead of Dynamic
    def readFile(name: String): String = {
      fs.readFileSync(name).toString
    }

    readFile("src/test/resources/" + path)
  }

}
