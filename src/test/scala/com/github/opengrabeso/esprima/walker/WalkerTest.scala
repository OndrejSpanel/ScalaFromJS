package net.gamatron.esprima.walker

import _root_.esprima.Node
import _root_.esprima.Node._
import scala.reflect.runtime.universe.{Literal => _,_}

import org.scalatest.FunSuite

class WalkerTest extends FunSuite {

  trait Counter {
    var count = 0
  }
  def createCounter = new Counter with (Node => Unit) {
    def apply(node: Node) = {
      count += 1
    }
  }

  def createLValue = new UnaryExpression("", null)
  def createRValue = new Literal("0", "0")

  def testForObject[T <: Node: TypeTag](node: T, expectedCount: Int) = {
    val walker = createWalkerForNode(implicitly[TypeTag[T]])
    val counter = createCounter
    walkNode(node, walker, counter)
    assert(counter.count == expectedCount)
  }

  test("Create walker for expression with no subnodes") {
    val o = new DebuggerStatement()
    testForObject(o, 0)
  }

  test("Create walker for expression with simple subnodes") {
    val o = new AssignmentExpression("=", createLValue, createRValue)
    testForObject(o, 2)
  }

  test("Create walker for expression with Array of subnodes") {
    val o = new ArrayExpression(Array(createRValue, createRValue, createRValue))
    testForObject(o, 3)
  }

  test("Create all walkers") {
    val walkers = createAllWalkers
    assert(walkers.size > 75) // as of Dec 2017 there are 77 classes derived from Node. It is unlikely the number will be ever lower.
    // check that walkers for a few randomly picked up classes exists and is reasonable
    assert(walkers contains classOf[AssignmentExpression])
    assert(walkers contains classOf[EmptyStatement])
    assert(walkers contains classOf[Program])
    assert(walkers contains classOf[Module])
  }
}
