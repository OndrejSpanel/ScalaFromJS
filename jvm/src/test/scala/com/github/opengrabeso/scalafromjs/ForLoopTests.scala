package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class ForLoopTests extends FunSuite with TestUtils {
  test("Detect for loop variable scope") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      function ForLoop() {
        var j, i;
        for ( j = 0; j <= 10; j ++ ) {
          for ( i = 0; i <= 20; i ++ ) {}

        }

        for (j = j + 1; j <= 30; j ++ ) {
          for ( i = 1; i <= 40; i ++ ) {}
        }

        i = i + 1;

        for (j = j + 1; j <= 50; j ++ ) {
           for ( i = 1; i <= 60; i ++ ) {}
        }

        console.log("j");
      }
      """).required(
        "while (j <= 10)",
        "for (i <- 0 to 20)",
        "while (j <= 30)",
        "while (i <= 40)",
        "while (j <= 50)",
        "for (i <- 1 to 60)"
      )
  }

  test("Detect for loops with limit variable") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var i,il;
      for ( i = 0, il = c.count; i < il; i ++ ) {}
      """).required(
        "for (i <- 0 until c.count)"
      )
  }

  test("Detect for loop as indices") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var i, sum = 0;
      for ( i = 0; i < c.length; i++ ) { sum += i}
      """).required(
        "for (i <- c.indices)"
      ).forbidden(
        "length"
      )

  }

}
