package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.ConvertProject._
import esprima.NodeExtended

object TypesRule {
  def transform(n: NodeExtended): NodeExtended = {
    val rules = n.config.collectRules[TypesRule]
    rules.foldLeft(n)((c, r) => r.provideTypes(c))
    n
  }
}
case class TypesRule(types: String) extends ExternalRule {
  // load the d.ts
  // create a separate project for it, the result is the types only


  // we cannot use normal apply, as we need to be called at a specific stage
  def provideTypes(c: NodeExtended) = {
    c
  }
}
