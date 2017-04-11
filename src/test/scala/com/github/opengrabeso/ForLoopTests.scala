package com.github.opengrabeso

import org.scalatest.FunSuite

class ForLoopTests extends FunSuite with TestUtils {
  test("Detect for loop variable scope") {
    execute check ConversionCheck(
      //languge Javascript
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
}