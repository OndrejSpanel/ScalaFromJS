package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._

object Collections {


  def usedOnlyAsIndex(body: AST_Node, varName: SymbolDef, objName: SymbolDef): Boolean = {
    var otherUse = false
    body.walk {
      case node@AST_SymbolRefDef(`objName`) AST_Sub AST_SymbolRefDef(`varName`) =>
        //println(s"Allowed use $node")
        true
      case node@AST_SymbolRefDef(`varName`) =>
        //println(s"Forbidden use $node")
        otherUse = true
        false
      case _ =>
        false
    }
    !otherUse
  }

  def apply(n: AST_Node): AST_Node = {

    n.transformAfter { (node, _) =>
      node match {

        // TODO: detect also more complex expressions than object.length, esp. like this.member.object.length
        case forStatement@ForRange(varName, "until", initVar@AST_Number(0), (obj@AST_SymbolRefDef(objName)) AST_Dot "length", AST_Number(1))
          if usedOnlyAsIndex(forStatement.body, varName, objName) =>
          // note: AST_ForOf would be more appropriate, however it is not present yet in the Uglify AST we use
          new AST_ForIn {
            fillTokens(this, node)
            this.`object` = obj
            this.init = AST_Let(node)(AST_VarDef.uninitialized(node)(varName.name))
            this.name = AST_SymbolRef.symDef(node)(varName)
            // TODO: transform index access in the body
            this.body = forStatement.body
          }
        case _ =>
          node
      }
    }
  }
}
