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

  case class TestSetup(mustHave: Seq[String] = Seq.empty[String], mustNotHave: Seq[String] = ConversionCheck.standardForbidden)

  case class TestCheck(setup: TestSetup = TestSetup(), getResult: () => String) {

    def requiredNothing: TestCheck = copy(setup = setup.copy(mustHave = Seq()))
    def forbiddenNothing: TestCheck = copy(setup = setup.copy(mustNotHave = Seq()))

    def required(add: String*) = copy(setup = setup.copy(mustHave = setup.mustHave ++ add))
    def forbidden(add: String*) = copy(setup = setup.copy(mustNotHave = setup.mustNotHave ++ add))

    def checkResult(result: String): Try[Unit] = {
      val missing = setup.mustHave.filter(s => !result.contains(normalizeEol(s)))
      val forbidden = setup.mustNotHave.filter(s => result.contains(normalizeEol(s)))
      if (missing.isEmpty & forbidden.isEmpty) {
        Success(())
      } else Failure {
        def stringList(ss: Seq[String]) = ss.map("  " + _ + "\n").mkString

        val missingStr = if (missing.nonEmpty) "Missing: \n" + stringList(missing) else ""
        val forbiddenStr = if (forbidden.nonEmpty) "Forbidden: \n" + stringList(forbidden) else ""

        new UnsupportedOperationException(missingStr + forbiddenStr + "in \n" + result)
      }
    }

    def execute() = {
      checkResult(getResult()).failed.foreach(throw _)
    }
  }

  def ResultCheck(result: String) = TestCheck(getResult = () => result)

  def ConversionCheck(code: String) = TestCheck(getResult = () => Convert(code))

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

  object execute {
    def check(setup: TestCheck) = setup.execute()
  }


}
