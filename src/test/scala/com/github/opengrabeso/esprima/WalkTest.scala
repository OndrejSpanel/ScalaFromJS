package com.github.opengrabeso.esprima

import esprima.Esprima._
import esprima.Node._
import org.scalatest.FunSuite

import scala.collection.mutable

class WalkTest extends FunSuite with TestInputs {

  def countASTNodes(ast: Node): (Int, mutable.Map[Class[_], Int]) = {
    var count = 0
    val countNodes = collection.mutable.Map.empty[Class[_], Int].withDefaultValue(0)
    ast.walk { node =>
      count += 1
      countNodes(node.getClass) += 1
      false
    }
    println(s"Total: $count")
    println("  " + countNodes.toSeq.map { case (k, v) => k.getSimpleName -> v }.sortBy(_._2).reverse.take(10).mkString("\n  "))
    (count, countNodes)
  }


  test("Walk simple expression") {
    val ast = parse(answer42)
    var hit = false
    ast.walk {
      case node: AssignmentExpression =>
        hit = true
        false
      case node =>
        false
    }
    assert(hit)
  }

  test("Walk complex expression") {
    val ast = parse(es6)
    var countBinary = 0
    var countOther = 0
    ast.walk {
      case _: BinaryExpression =>
        countBinary += 1
        false
      case _ =>
        countOther += 1
        false
    }
    assert(countBinary >= 7)
    assert(countOther >= 100)
  }

  test("Walk esprima.js") {
    val ast = parse(esprimaSource)
    var (count, countNodes) = countASTNodes(ast)
    // verify node counts are sensible (we are really walking the tree)
    assert(countNodes(classOf[Identifier]) >= 10000)
    assert(countNodes(classOf[StaticMemberExpression]) >= 5000)
    assert(countNodes(classOf[FunctionExpression]) >= 100)
    assert(count >= 25000)
  }

  test("Walk three.js") {
    val ast = parse(threeSource)
    var (count, countNodes) = countASTNodes(ast)
    // verify node counts are sensible (we are really walking the tree)
    assert(countNodes(classOf[Identifier]) >= 50000)
    assert(countNodes(classOf[StaticMemberExpression]) >= 10000)
    assert(countNodes(classOf[FunctionExpression]) >= 1000)
    assert(count >= 100000)
  }


  private def verifyTransformResults(transformed: Node) = {
    var oldHits = 0
    var newHits = 0
    transformed.walk {
      case Identifier("answer") =>
        oldHits += 1
        false
      case Identifier("question") =>
        newHits += 1
        false
      case Literal(esprima.OrType(42), _) =>
        oldHits += 1
        false
      case Literal(esprima.OrType(24), _) =>
        newHits += 1
        false
      case _ =>
        false
    }
    assert(oldHits == 0)
    assert(newHits == 2)
  }

  test("transformAfter") {
    val ast = parse(answer42)

    val transformed = ast.transformAfterSimple {
      case Identifier("answer") =>
        Identifier("question")
      case Literal(esprima.OrType(42), _) =>
        Literal(24, "24")
      case node =>
        node
    }

    verifyTransformResults(transformed)
  }

  test("transformBefore") {
    val ast = parse(answer42)

    val transformed = ast.transformBefore { (node, descend, transformer) =>
      val cloned = node.cloneNode()
      val transformed = cloned match {
        case Identifier("answer") =>
          Identifier("question")
        case Literal(esprima.OrType(42), _) =>
          Literal(24, "24")
        case _ =>
          cloned
      }

      descend(transformed, transformer)
      transformed
    }

    verifyTransformResults(transformed)

  }


}
