package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._

object Collections {


  def usedOnlyAsIndex(body: AST_Node, varName: SymbolDef, objName: SymbolDef): Boolean = {
    object IndexUsage {
      def unapply(arg: AST_Sub) = arg match {
        case node@AST_SymbolRefDef(`objName`) AST_Sub AST_SymbolRefDef(`varName`) =>
          true
        case _ =>
          false
      }
    }
    var otherUse = false
    body.walk {
      case node@AST_Assign(IndexUsage(), _, _) =>
        // use on the left side of assignment - that is not allowed
        //println(s"L-value use $node")
        otherUse = true
        otherUse
      case node@IndexUsage() =>
        //println(s"Allowed use $node")
        true
      case node@AST_SymbolRefDef(`varName`) =>
        //println(s"Forbidden use $node")
        otherUse = true
        otherUse
      case _ =>
        otherUse
    }
    !otherUse
  }

  def transformIndexUse(body: AST_Node, varName: SymbolDef, objName: SymbolDef): AST_Node = {
    body.transformAfter {(node, _) =>
      node match {
        case AST_SymbolRefDef(`objName`) AST_Sub (varRef@AST_SymbolRefDef(`varName`)) =>
          varRef
        case _ =>
          node

      }
    }
  }

  def substituteIndex(forStatement: AST_ForIn, varName: SymbolDef) = {
    // if there is a single variable inside of the body as const xx = varName and varName it not used otherwise, substitute it
    var otherUse = false
    var subst = Option.empty[SymbolDef]
    forStatement.body.walk {
      case AST_Const(AST_VarDef(AST_SymbolDef(name), Defined(AST_SymbolRefDef(`varName`)))) =>
        subst = Some(name)
        true
      case AST_SymbolRefDef(`varName`) =>
        otherUse = true
        otherUse
      case _ =>
        otherUse

    }
    for {
      substName <- subst if !otherUse
    } {
      //println(s"Detected substitution of ${varName.name} as ${substName.name}")
      Variables.renameVariable(forStatement.body, varName, substName.name, substName)
      // the body now contains "const substName = substName, remove it
      forStatement.body = forStatement.body.transformAfter {(node, _) =>
        node match {
          case AST_Const(AST_VarDef(AST_SymbolDef(`substName`), Defined(AST_SymbolRefDef(`substName`)))) =>
            AST_EmptyStatement(node)
          case _ =>
            node
        }
      }
      Variables.renameVariable(forStatement.init, varName, substName.name, substName)
      forStatement.name = AST_SymbolRef.symDef(forStatement.init)(substName)
    }
  }


  def transformFor(forStatement: AST_ForIn, varName: SymbolDef, objName: SymbolDef): AST_ForIn = {
    transformIndexUse(forStatement.body, varName, objName)
    substituteIndex(forStatement, varName)
    forStatement
  }
  def apply(n: AST_Node): AST_Node = {

    n.transformAfter { (node, _) =>
      node match {

        // TODO: detect also more complex expressions than object.length, esp. like this.member.object.length
        case forStatement@ForRange(varName, "until", initVar@AST_Number(0), (obj@AST_SymbolRefDef(objName)) AST_Dot "length", AST_Number(1))
          if usedOnlyAsIndex(forStatement.body, varName, objName) =>
          // note: AST_ForOf would be more appropriate, however it is not present yet in the Uglify AST we use
          //println(s"Detect for ..in $forStatement")
          val newFor = new AST_ForIn {
            fillTokens(this, node)
            this.`object` = obj
            this.init = AST_Let(node)(AST_VarDef.uninitialized(node)(varName.name))
            this.name = AST_SymbolRef.symDef(node)(varName)
            this.body = forStatement.body
          }
          transformFor(newFor, varName, objName).asInstanceOf[AST_Statement]
        case _ =>
          node
      }
    }
  }
}
