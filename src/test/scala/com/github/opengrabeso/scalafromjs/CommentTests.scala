package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class CommentTests extends FunSuite with TestUtils {
  test("Complex single line comments should be processed correctly") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      function f() {

        var x;

        // line 1
        // line 2

        // line A
        callFunction();

        // line B

        if (true) x = 4

      }
      """)
      .custom { result =>
        def checkOccurences(string: String, expectedCount: Int = 1) = {
          val count = string.r.findAllMatchIn(result).size
          if (count != expectedCount) Seq(s"Expected $expectedCount occurences of '$string', found $count")
          else Seq.empty
        }

        checkOccurences("line 1") ++
          checkOccurences("line 2") ++
          checkOccurences("line A") ++
          checkOccurences("line B")


      }
  }

  test("Trailing single line comments in a block should be formatted correctly") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      // start file
      var a = "";

      if ( a != "x" ) { // start block
        a = "y"; // middle of the block
        a = a + a; // end block
      } // after block
      // another line after
      """)
      .required(
        "// start file",
        "{ // start block",
        "a = \"y\" // middle of the block",
        "a = a + a // end block",
        "} // after block",
        "// another line after"
      )
  }

}

