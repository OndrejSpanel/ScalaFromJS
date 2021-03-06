package com.github.opengrabeso.scalafromjs.esprima
package symbols

import org.scalatest.funsuite.AnyFunSuite
import com.github.opengrabeso.esprima.Esprima._

class SymbolsTest extends AnyFunSuite with TestInputs {

  test("Walk with scope tracking ") {
    val ast = parse(es6)
    var maxScopeDepth = 0
    var maxNodeDepth = 0
    walk(ast) { (node, context) =>
      maxScopeDepth = maxScopeDepth max context.scopes.length
      maxNodeDepth = maxNodeDepth max context.parents.length
      false
    }
    println(s"Max scope depth $maxScopeDepth")
    println(s"Max node depth $maxNodeDepth")
    assert(maxScopeDepth > 0)
    assert(maxNodeDepth > 0)
  }

  test("Walk three.js with scope tracking ") {
    val ast = parse(threeSource)
    var maxScopeDepth = 0
    var maxNodeDepth = 0
    walk(ast) { (node, context) =>
      maxScopeDepth = maxScopeDepth max context.scopes.length
      maxNodeDepth = maxNodeDepth max context.parents.length
      false
    }
    println(s"Max scope depth $maxScopeDepth")
    println(s"Max node depth $maxNodeDepth")
    assert(maxScopeDepth > 0)
    assert(maxNodeDepth > 0)
  }

  test("Collect all symbols correctly") {
    val ast = parse(es6)
    val symbols = listAllSymbols(ast)
    val nonGlobalNames = symbols.filter(_.sourcePos._1 >= 0).map(_.name)
    val known = Set("Identifier", "Literal", "useIfSimple", "key", "value", "i")
    val unknown = known -- nonGlobalNames
    assert(unknown.isEmpty)
    //println(symbols)
  }
}
