package com.github.opengrabeso

import scala.scalajs.js
import Uglify._
import UglifyExt._
import JsUtils._
import js.JSConverters._

/**
  * Transform AST to perform optimizations or adjustments
  * */
object Transform {

  // low-level operations, used to implement the rest

  def add(n: AST_Node) = ???
  def remove(n: AST_Node) = ???

  def checkAssignToReference(s: AST_SimpleStatement, df: SymbolDef) = {
    require(!s.body.isInstanceOf[AST_Statement])
    s.body match {
      case a: AST_Assign =>
        a.left match {
          case sym: AST_SymbolRef =>
            sym.thedef.exists(_ == df)
          case _ =>
            false
        }
      case a =>
        false
    }
  }

  def checkUnaryToReference(s: AST_Unary, df: SymbolDef) = {
    s.expression match {
      case sym: AST_SymbolRef =>
        (s.operator == "++" || s.operator == "--") && sym.thedef.exists(_ == df)
      case _ =>
        false
    }
  }

  // individual sensible transformations

  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Node): AST_Node = {
    val ret = n.clone()
    // walk the tree, check for possible val replacements and perform them
    ret.walk{ node: AST_Node =>
      node match {
        case v: AST_VarDef =>
          for (df <- v.name.thedef) {
            assert(df.name == v.name.name)
            if (v.name.init.nonNull.isEmpty) {
              // without init it must be var -
              // TODO: infer type

              // check if any reference is assignment target
              df._isVal = !df.references.exists { ref =>
                assert(ref.thedef.exists(_ == df))
                ref.scope.exists{ s =>
                  var detect = false
                  //noinspection MatchToPartialFunction
                  s.walk { node: AST_Node =>
                    node match {
                      case s: AST_Scope => s != ref.scope // do not descend into any other scopes, they are listed in references if needed
                      case ss: AST_SimpleStatement =>
                        if (checkAssignToReference(ss, df)) detect = true
                        detect
                      case u: AST_Unary =>
                        if (checkUnaryToReference(u, df)) detect = true
                        detect
                      case _ =>
                        detect
                    }
                  }
                  detect
                }
              }
            }
          }
        case _ =>
      }
      false
    }
    ret
  }

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Node): AST_Node = {
    // walk the tree, check for possible val replacements and perform them
    val ret = n.transform { (node, descend, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      node match {
        case b: AST_Block =>
          println(b.body.map(nodeClassName).mkString("[",",","]"))
          println("  " + b.body.length)
          println(ScalaOut.output(b, ""))
          val c = b.clone().asInstanceOf[AST_Block]
          val newBody = for (s <- c.body) yield {
            println(nodeClassName(s))
            val sr = s match {
              case ss: AST_SimpleStatement =>
                ss.clone().asInstanceOf[AST_Statement] // descending into AST_SimpleStatement causes TypeError: node.transform is not a function
              case _ =>
                s.clone().asInstanceOf[AST_Statement]
            }
            println("SR " + nodeClassName(sr))
            sr
          }
          println(newBody.map(nodeClassName).mkString("new [",",","]"))
          c.body = newBody.toJSArray
          descend(c, transformer)
          c
        case c =>
          println(nodeClassName(c))
          println(ScalaOut.outputNode(c, ""))
          val cc = node.clone()
          descend(cc, transformer)
          cc
      }
    }
    ret
  }

  def apply(n: AST_Toplevel): AST_Toplevel = {
    val init = varInitialization(n).asInstanceOf[AST_Toplevel]
    val vals = detectVals(init).asInstanceOf[AST_Toplevel]
    vals
  }
}
