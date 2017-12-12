package net.gamatron.esprima
package symbols

import org.scalatest.FunSuite
import esprima.Esprima._

class SymbolsTest extends FunSuite with TestInputs {

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
    val nonGlobalNames = symbols.filter(_.scope >= 0).map(_.name)
    val known = Set("Identifier", "Literal", "useIfSimple", "key", "value", "i")
    val unknown = known -- nonGlobalNames
    assert(unknown.isEmpty)
    //println(symbols)
  }
}
