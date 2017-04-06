package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._

import scala.language.implicitConversions

object VariableUtils {
  trait Extractor[X] {
    def unapply(arg: AST_Node): Option[X]
  }

  class IsModified[X](extract: Extractor[X]) extends Extractor[X] {
    def unapply(arg: AST_Node): Option[X] = arg match {
      case AST_Assign(extract(x), _, _) =>
        //println(s"  Detected assignment modification of ${df.name}")
        Some(x)
      case AST_Unary(UnaryModification(), extract(x)) =>
        Some(x)
      case _ =>
        None
    }
  }


  case class ReferenceScopes(refs: Map[SymbolDef, Set[AST_Scope]]) {
    def walkReferences[X](df: SymbolDef, isDfModified: Extractor[X])(onModification: X => Boolean): Boolean = {
      // ++ orig is a hotfix for issue https://github.com/mishoo/UglifyJS2/issues/1702 - include orig, likely to help
      val scopes = refs.getOrElse(df, Seq())

      scopes.exists { s =>
        //println(s"Reference to ${df.name} in scope ${scopeName(s)}")
        var abort = false
        s.walk {
          case ss: AST_Scope =>
            ss != s // do not descend into any other scopes, they are listed in references if needed
          case isDfModified(x) =>
            //println(s"  Detected modification of ${df.name}")
            if (onModification(x)) abort = true
            abort
          case _ =>
            abort
        }
        abort
      }
    }
  }

  def buildReferenceStacks(n: AST_Node) = {

    Time("buildReferenceStacks") {
      var refs = Map.empty[SymbolDef, Set[AST_Scope]]

      n.walkWithDescend { (node, _, walker) =>
        node match {
          case AST_SymbolRefDef(symDef) =>
            val old = refs.getOrElse(symDef, Set())

            val stack = walker.stack.toSeq.collect {
              case s: AST_Scope => s
            }

            refs += symDef -> (old ++ stack)
          case _ =>
        }

        false // descend(node, walker)
      }

      ReferenceScopes(refs)
    }
  }


}
