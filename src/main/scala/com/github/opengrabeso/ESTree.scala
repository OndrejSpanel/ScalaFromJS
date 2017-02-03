package com.github.opengrabeso

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

/*
Follow https://github.com/estree/estree/blob/master/es2015.md
 */
@js.native
@JSGlobalScope
object ESTree extends js.Any {

  @js.native
  trait Node extends js.Object {
    val `type`: String = js.native
    val expression: js.UndefOr[Node] = js.native
  }
  @js.native
  trait Program extends Node { // https://github.com/estree/estree/blob/master/es2015.md#programs
    val body: js.Array[Node] = js.native
    val sourceType: String  = js.native // "script" | "module"
  }

  @js.native
  trait AssignmentExpression extends Node {
    val `operator`: String = js.native
    val left: Node = js.native
    val right: Node = js.native
  }

}
