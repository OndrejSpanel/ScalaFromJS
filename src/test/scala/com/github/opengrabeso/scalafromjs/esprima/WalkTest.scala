package com.github.opengrabeso.scalafromjs.esprima

import com.github.opengrabeso.esprima._
import com.github.opengrabeso.esprima.Node._
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.language.implicitConversions

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
    assert(countNodes(classOf[Identifier]) >= 5000)
    assert(countNodes(classOf[StaticMemberExpression]) >= 5000)
    assert(countNodes(classOf[FunctionExpression]) >= 100)
    assert(count >= 25000)
  }

  test("Walk three.js") {
    val ast = parse(threeSource)
    var (count, countNodes) = countASTNodes(ast)
    // verify node counts are sensible (we are really walking the tree)
    assert(countNodes(classOf[Identifier]) >= 40000)
    assert(countNodes(classOf[StaticMemberExpression]) >= 10000)
    assert(countNodes(classOf[FunctionExpression]) >= 1000)
    assert(count >= 100000)
  }

  implicit def toPair(node: Node): (Node, Node) = node -> node

  private def transformAnswer42(node: Node): (Node, Node) = node match {
    case Identifier("answer") =>
      Identifier("question").withTokens(node)
    case Literal(OrType(42), _) =>
      Literal(24, "24")
    case AssignmentExpression(op, left, right) =>
      AssignmentExpression("+=", left, right)
    case node@ExpressionStatement(_) =>
      BlockStatement(Seq(node)).withTokens(node) -> node
    case _ =>
      node

  }

  private def verifyTransformResults(transformed: Node, isOld: Boolean) = {
    var oldHits = 0
    var newHits = 0
    transformed.walk {
      case Identifier("answer") =>
        oldHits += 1
        false
      case Identifier("question") =>
        newHits += 1
        false
      case Literal(OrType(42), _) =>
        oldHits += 1
        false
      case Literal(OrType(24), _) =>
        newHits += 1
        false
      case AssignmentExpression("=", _, _) =>
        oldHits += 1
        false
      case AssignmentExpression("+=", _, _) =>
        newHits += 1
        false
      case _ =>
        false
    }
    val totalHits = 3
    if (isOld) {
      assert(oldHits == totalHits)
      assert(newHits == 0)
    } else {
      assert(oldHits == 0)
      assert(newHits == totalHits)
    }
  }

  test("transformAfter") {
    val ast = parse(answer42)

    verifyTransformResults(ast, true)

    val transformed = ast.transformAfterSimple(transformAnswer42(_)._1)

    verifyTransformResults(transformed, false)
  }

  test("transformBefore") {
    val ast = parse(answer42)

    val transformed = ast.transformBefore { (node, descend, transformer) =>
      val cloned = node.cloneNode()
      val (transformed, descendInto) = transformAnswer42(cloned)

      // prevent recursion when e.g. ExpressionStatement is enclosed in a BlockStatement
      // not a general solution, prevents descending into any modified nodes
      descend(descendInto, transformer)
      transformed
    }

    verifyTransformResults(ast, true)
    verifyTransformResults(transformed, false)
  }


}
