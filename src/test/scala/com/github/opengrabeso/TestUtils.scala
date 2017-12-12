package com.github.opengrabeso

import org.scalatest.Assertions

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait TestUtils extends Assertions {
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

    def exec() = {
      checkResult(getResult()).failed.foreach(x => fail(x.getMessage))
    }
  }

  def ResultCheck(result: String) = TestCheck(getResult = () => result)

  def ProjectCheck(path: String) = TestCheck(getResult = () => Convert.project(path))

  def ConversionCheck(code: String) = TestCheck(getResult = () => Convert(code))

  def rscPath(path: String): String = "src/test/resources/" + path

  def rsc(path: String) = {

    // TODO: facade instead of Dynamic
    def readFile(name: String): String = {
      ???
    }

    readFile(rscPath(path))
  }

  object exec {
    def check(setup: TestCheck) = setup.exec()
  }


}
