package com.github.opengrabeso
package transform

import com.github.opengrabeso.esprima._
import _root_.esprima._

import Expressions._

import scala.language.implicitConversions

object VariableUtils {
  trait Extractor[X] {
    def unapply(arg: Node.Node): Option[X]
  }

  class IsModified[X](extract: Extractor[X]) extends Extractor[X] {
    def unapply(arg: Node.Node): Option[X] = {
      //println(s"Check modification of $arg")
      arg match {
        case Node.Assign(extract(x), _, _) =>
          //println(s"  Detected assignment modification of ${df.name}")
          Some(x)
        case Node.UnaryExpression(UnaryModification(), extract(x)) =>
          Some(x)
        case _ =>
          None
      }
    }
  }

  class IsSym(sym: SymbolDef) extends Extractor[Unit] {
    def unapply(arg: Node.Node) = arg match {
      case Node.Identifier(`sym`) => Some(())
      case _ => None
    }
  }

  def listSymbols(n: Node.Node): Set[SymbolDef] = {
    val symbols = Set.newBuilder[SymbolDef]
    n walk {
      case Node.Identifier(symDef) =>
        symbols += symDef
        false
      case _ =>
        false
    }
    symbols.result()
  }

  class VarIsModified(sym: SymbolDef) extends IsModified(new IsSym(sym))

  case class ReferenceScopes(refs: Map[SymbolDef, Set[Node.Scope]]) {
    def walkReferences[X](df: SymbolDef, isDfModified: Extractor[X])(onModification: X => Boolean = (_: X) => true): Boolean = {
      val scopes = refs.getOrElse(df, Set())

      scopes.exists { s =>
        //println(s"Reference to ${df.name} in scope ${scopeName(s)}")
        var abort = false
        s.walk {
          case ss: Node.Scope =>
            //println(s"Enter scope $ss")
            ss != s // do not descend into any other scopes, they are listed in references if needed
          //noinspection ScalaUnusedSymbol
          case node@isDfModified(x) =>
            //println(s"  Detected modification of ${df.name} by $node")
            if (onModification(x)) abort = true
            abort
          case _ =>
            abort
        }
        abort
      }
    }

    def isModified(df: SymbolDef): Boolean = walkReferences(df, new VarIsModified(df))()
  }

  def buildReferenceStacks(n: Node.Node) = {

    Time.disabled("buildReferenceStacks") {
      var refs = Map.empty[SymbolDef, Set[Node.Scope]]

      n.walkWithDescend { (node, _, walker) =>
        node match {
          case Node.Identifier(symDef) =>
            val old = refs.getOrElse(symDef, Set())

            val stack = walker.stack.toSeq.collect {
              case s: Node.Scope => s
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
