package com.github.opengrabeso

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
@JSGlobalScope
object ESTree extends js.Any {

  @js.native
  trait Node extends js.Object {
    val body: js.Array[js.Any]
    val sourceType: String
    val `type`: String
  }

}
