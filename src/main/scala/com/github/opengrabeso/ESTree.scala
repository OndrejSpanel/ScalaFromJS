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
    val `type`: String
  }
  @js.native
  trait Program extends Node { // https://github.com/estree/estree/blob/master/es2015.md#programs
    val body: js.Array[Node]
    val sourceType: String // "script" | "module"
  }

}
