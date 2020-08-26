package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class NumberTypeTests extends AnyFunSuite with TestUtils {
  test("Autodetect hex color values") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var number1 = 1;
      var number_red = 0xff0000;
      var number_color = 0x448822;
      var number_small = 10;
      var number_double = 10.2;
      var number_large = 123456789123;
      var number_double_sci = 1e-1;
      var number_double_sci2 = 1e2;
      """)
      .required(
        "number1 = 1",
        "number_red = 0xff0000",
        "number_color = 0x448822",
        "number_small = 10",
        "number_double = 10.2",
        "number_large = 123456789123",
        "number_double_sci = 1e-1",
        "number_double_sci2 = 1e2"
      )
  }
}