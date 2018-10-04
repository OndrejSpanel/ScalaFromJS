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

  test("Trailing single line comments in a block") {
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


  test("Comments in parameter lists") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      function f(/*start*/ param /* end*/ ) {
        return param;
      }

      function C(/*c-start*/ param /* c-end*/ ) {
        return param;
      }

      var c = new C(0)
      """)
      .required(
        "def f( /*start*/param",
        "/*end*/)",
        "class C( /*c-start*/param",
        "/*c-end*/)"
      )
  }

  test("Comments in class body") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      /** Comment before a class constructor */
      function C() {}

      /** Comment before a class prototype */
      C.prototype = Object.assign( Object.create( B.prototype ), {
        /** comment before a function */
        f: function() {
          // comment in a function
        }
        /* comment after a function */
      });

      var c = new C()
      """)
      .required(
        "before a class constructor",
        "before a function",
        "after a function"
      )
  }
}

