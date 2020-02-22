package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class ObjectTests extends FunSuite with TestUtils {
  test("Object literals should be converted") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var obj = {a: "A", b: "B"};
      """)
      .required(
        """object obj""",
        """var a = "A"""",
        """var b = "B""""
      )
  }

  test("Object literals with string keys should be converted") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var obj = {"aa": "A", "-b": "B"};
      """)
      .required(
        """object obj""",
        """var aa = "A"""",
        """var `-b` = "B""""
      )
  }

  test("Object literals with numeric keys should be converted") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var obj = {0: "A", 0.5: "B", 1.0: "C"};
      """)
      .required(
        """object obj""",
        """var `0` = "A"""",
        """var `0.5` = "B"""",
        """var `1` = "C""""
      )
  }

}
