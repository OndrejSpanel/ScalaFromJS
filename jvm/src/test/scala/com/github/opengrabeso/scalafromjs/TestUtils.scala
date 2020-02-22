package com.github.opengrabeso.scalafromjs

import org.scalatest.Assertions

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait TestUtils extends Assertions {
  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  def normalizeEol(str: String) = str.replace("\r\n", "\n")

  object ConversionCheck {
    val standardForbidden = Seq(";", "/* Unsupported:", "??? /*null*/")
  }

  case class TestSetup(
    mustHave: Seq[String] = Seq.empty[String],
    mustHaveOrdered: Seq[String] = Seq.empty[String],
    mustNotHave: Seq[String] = ConversionCheck.standardForbidden,
    custom: String => Seq[String] = _ => Seq.empty
  )

  case class TestCheck(setup: TestSetup = TestSetup(), getResult: () => String) {

    def requiredNothing: TestCheck = copy(setup = setup.copy(mustHave = Seq()))
    def forbiddenNothing: TestCheck = copy(setup = setup.copy(mustNotHave = Seq()))

    def required(add: String*) = copy(setup = setup.copy(mustHave = setup.mustHave ++ add))
    def requiredInOrder(add: String*) = copy(setup = setup.copy(mustHaveOrdered = setup.mustHaveOrdered ++ add))
    def forbidden(add: String*) = copy(setup = setup.copy(mustNotHave = setup.mustNotHave ++ add))
    def custom(fun: String => Seq[String]) = copy(setup = setup.copy(custom = fun))

    def checkResult(result: String): Try[Unit] = {
      val customResult = setup.custom(result)
      val missing = setup.mustHave.filter(s => !result.contains(normalizeEol(s)))

      @scala.annotation.tailrec
      def findInOrder(todo: Seq[String], in: String, missing: Seq[String]): Seq[String] = {
        todo match {
          case head +: tail =>
            val where = in.indexOf(head)
            if (where >= 0) {
              findInOrder(tail, in.drop(where + head.length), missing)
            } else {
              findInOrder(tail, in, head +: missing)
            }
          case _ =>
            missing
        }
      }

      val missingInOrder = findInOrder(setup.mustHaveOrdered, result, Nil)

      val forbidden = setup.mustNotHave.filter(s => result.contains(normalizeEol(s)))
      if (missing.isEmpty && forbidden.isEmpty && customResult.isEmpty && missingInOrder.isEmpty) {
        Success(())
      } else Failure {
        def stringList(ss: Seq[String]) = ss.map("  " + _ + "\n").mkString

        val missingStr = if (missing.nonEmpty) "Missing: \n" + stringList(missing) else ""
        val missingInOrderStr = if (missingInOrder.nonEmpty) "Missing in order: \n" + stringList(missingInOrder) else ""
        val forbiddenStr = if (forbidden.nonEmpty) "Forbidden: \n" + stringList(forbidden) else ""
        val customStr = if (customResult.nonEmpty) "Failed: \n" + stringList(customResult) else ""

        new UnsupportedOperationException(missingStr + missingInOrderStr + forbiddenStr + customStr + "in \n" + result)
      }
    }

    def exec() = {
      checkResult(getResult()).failed.foreach(x => fail(x.getMessage))
    }
  }

  def ResultCheck(result: String) = TestCheck(getResult = () => result)

  def ProjectCheck(path: String) = TestCheck(getResult = () => Convert.project(path))

  def ConversionCheck(code: String) = TestCheck(getResult = () => Convert(code))
  def ConversionCheckTypeScript(code: String) = TestCheck(getResult = () => Convert(code, typescript = true))

  def rscPath(path: String): String = "jvm/src/test/resources/" + path

  def rsc(path: String) = {

    def readFile(name: String): String = {
      val in = io.Source.fromInputStream(getClass.getResourceAsStream("/" + name))
      in.mkString
    }

    readFile(path)
  }

  object exec {
    def check(setup: TestCheck) = setup.exec()
  }


}
