package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class SplitFilesTests extends FunSuite with TestUtils {
  test("Simulated multiple file conversion") {
    exec check ConversionCheck(
      """
         //file:a.js
         // Some Javascript file
         function a() {}

         //file:b.js
         // Another Javascript file
         import "a.js"
         function b() {}
      """).required(
      "def a()",
      "def b()",
      "/* import \"a.js\" */"
    )

  }


}
