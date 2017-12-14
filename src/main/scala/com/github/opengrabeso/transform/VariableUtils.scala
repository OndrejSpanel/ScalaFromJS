package com.github.opengrabeso
package transform

import com.github.opengrabeso.esprima._
import _root_.esprima._
import Expressions._
import com.github.opengrabeso.esprima.symbols.{Id, ScopeContext, SymId}

import scala.language.implicitConversions

object VariableUtils {
  trait Extractor[X] {
    def unapply(arg: Node.Node): Option[X]
  }

  class IsModified[X](extract: Extractor[X]) extends Extractor[X] {
    def unapply(arg: Node.Node): Option[X] = {
      //println(s"Check modification of $arg")
      arg match {
        case Assign(extract(x), _, _) =>
          //println(s"  Detected assignment modification of ${df.name}")
          Some(x)
        case Node.UpdateExpression(_, extract(x), _) =>
          Some(x)
        case Node.UnaryExpression(UnaryModification(), extract(x)) =>
          Some(x)
        case _ =>
          None
      }
    }
  }

  class IsSym(sym: SymId) extends Extractor[Unit] {
    def unapply(arg: Node.Node) = arg match {
      // no need to check the scope, we are already traversing only scopes where the symbol is known to exist
      case Node.Identifier(name) if name == sym.name =>
        Some(())
      case _ => None
    }
  }

  def listSymbols(n: Node.Node)(implicit context: ScopeContext): Set[SymId] = {
    val symbols = Set.newBuilder[SymId]
    n walk {
      case Node.Identifier(Id(symDef)) =>
        symbols += symDef
        false
      case _ =>
        false
    }
    symbols.result()
  }

  class VarIsModified(sym: SymId) extends IsModified(new IsSym(sym))

  case class ReferenceScopes(refs: Map[SymId, Set[Node.IsScope]]) {
    def walkReferences[X](df: SymId, isDfModified: Extractor[X])(onModification: X => Boolean = (_: X) => true): Boolean = {
      val scopes = refs.getOrElse(df, Set())

      scopes.exists { s =>
        //println(s"Reference to ${df.name} in scope ${scopeName(s)}")
        var abort = false
        s.walk {
          case ss: Node.IsScope =>
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

    def isModified(df: SymId): Boolean = walkReferences(df, new VarIsModified(df))()
  }

  def buildReferenceStacks(n: Node.Node) = {

    Time.disabled("buildReferenceStacks") {
      var refs = Map.empty[SymId, Set[Node.IsScope]]

      n.walkWithScope { (node, walker) =>
        implicit val ctx = walker
        node match {
          case Node.Identifier(Id(symDef)) =>
            val old = refs.getOrElse(symDef, Set())

            val stack = walker.stack.collect {
              case s: Node.IsScope => s
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
