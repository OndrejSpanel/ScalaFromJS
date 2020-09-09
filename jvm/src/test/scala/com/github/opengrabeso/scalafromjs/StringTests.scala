package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class StringTests extends AnyFunSuite with TestUtils {
  test("Normal strings should be converted") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var hiThereDQ = "Hi, there, double quotes!";
      var hiThereSQ = 'Hi, there, single quotes!';
      var escapeDQ1 = 'Escape them 1: "';
      var escapeSQ1 = "Do not escape them 1: '";
      var escapeDQ2 = "'Escape them 2: \"";
      var escapeSQ2 = 'Do not escape them 2: \'';
      """)
      .required(
  """val hiThereDQ = "Hi, there, double quotes!"""",
        """val hiThereSQ = "Hi, there, single quotes!"""",
        """val escapeDQ1 = "Escape them 1: \""""",
        """val escapeSQ1 = "Do not escape them 1: '"""",
        """val escapeDQ2 = "'Escape them 2: \""""",
        """val escapeSQ2 = "Do not escape them 2: '""""
      )
  }

  ignore("UTF escapes in strings should be passed") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var escape1 = "\u000B";
      var escape2 = "\u2190";
      """)
      .required(
        "val escape1 = \"\\u000B\"",
        "val escape2 = \"\\u2190\""
      )

  }

  ignore("UTF characters in strings should be escaped") {
    // note: passing some characters through would be fine as well. If implementation is changed, tests can be adjusted.
    exec check ConversionCheck(
      //language=JavaScript
      """
      var arrow1 = "←";
      var arrow2 = "→";
      """)
      .required(
        "val arrow1 = \"\\u2190\"",
        "val arrow2 = \"\\u2192\""
      )
  }

  test("UTF characters in comments should be passed through") {
    // note: passing some characters through would be fine as well. If implementation is changed, tests can be adjusted.
    exec check ConversionCheck(
      //language=JavaScript
      """
      // go ← left
      // go → right
      // Esprima comment example: BindingElements ⊆ AssignmentTargets ⊆ AssignmentExpression
      var statementToFlushComments;
      """)
      .required(
        "go ← left",
        "go → right",
        "go ← left",
        "BindingElements ⊆ AssignmentTargets ⊆ AssignmentExpression"
      )
  }


}
