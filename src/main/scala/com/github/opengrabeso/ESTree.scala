package com.github.opengrabeso

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

/*
Follow https://github.com/estree/estree/blob/master/es5.md
 */
@js.native
@JSGlobalScope
object ESTree extends js.Any {

  @js.native
  trait Position extends js.Any {
    val line: Int = js.native
    val column: Int = js.native
  }

  @js.native
  trait SourceLocation extends js.Any {
    val source: js.UndefOr[String] = js.native
    val start: Position = js.native
    val end: Position = js.native
  }

  @js.native
  trait Node extends js.Object {
    val `type`: String = js.native
    val loc: js.UndefOr[SourceLocation] = js.native
  }

  @js.native
  trait Program extends Node { // https://github.com/estree/estree/blob/master/es2015.md#programs
    val body: js.Array[Node] = js.native
    val sourceType: String  = js.native // "script" | "module"
  }

  @js.native
  trait ExpressionStatement extends Node {
    val expression: Node = js.native
  }

  @js.native
  trait Expression extends Node

  @js.native
  trait AssignmentExpression extends Expression {
    val `operator`: String = js.native // AssignmentOperator
    val left: Node = js.native // Pattern | Expression
    val right: Expression = js.native
  }

  @js.native
  trait Literal extends Node {
    val value: js.Any = js.native
  }

  @js.native
  trait Identifier extends Node {
    val name: String = js.native
  }

}
