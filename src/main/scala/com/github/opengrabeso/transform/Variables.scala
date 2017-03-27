package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._

import scala.collection.mutable
import scala.scalajs.js
import js.JSConverters._
import scala.language.implicitConversions


object Variables {

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

  def walkReferences[X](df: SymbolDef, isDfModified: Extractor[X])(onModification: X => Boolean): Boolean = {
    // ++ orig is a hotfix for issue https://github.com/mishoo/UglifyJS2/issues/1702 - include orig, likely to help
    val scopes = (df.references ++ df.orig).flatMap { ref =>
      assert(ref.thedef contains df)
      ref.scope.nonNull
    }.toSet

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


  // detect variables which can be declared as val instead of var
  def detectVals(n: AST_Node): AST_Node = {
    // walk the tree, check for possible val replacements and perform them
    n.transformBefore {(node, descend, transformer) =>
      node match {
        case cm: AST_ConciseMethod =>
          if (cm.key.name != inlineBodyName) {
            // no var detection inside of inline class body (its variables are actually class members)
            val n = cm.clone()
            descend(n, transformer)
            n
          } else cm.clone()

        case AST_Var(varDef@AST_VarDef(varName, value)) if value.nonNull.nonEmpty => // var with init - search for a modification
          //println(s"AST_VarDef ${varName.name}")
          varName.thedef.fold(node) { df =>
            assert(df.name == varName.name)
            // check if any reference is assignment target

            object IsDf extends Extractor[Unit] {
              def unapply(arg: AST_Node) = arg match {
                case AST_SymbolRefDef(`df`) => Some(())
                case _ => None
              }
            }
            object IsDfModified extends IsModified(IsDf)

            val assignedInto = walkReferences(df, IsDfModified)(_ => true)
            val n = if (!assignedInto) {
              val c = varDef.clone()
              c.name = new AST_SymbolConst {
                fillTokens(this, varName)
                init = varName.init
                name = varName.name
                scope = varName.scope
                thedef = varName.thedef
              }
              new AST_Const {
                fillTokens(this, node)
                definitions = js.Array(c)
              }
            } else {
              node.clone()
            }
            descend(n, transformer) // may contain other scopes with initializations
            n
          }
        case _ =>
          val n = node.clone()
          descend(n, transformer)
          n
      }
    }
  }

  // detect function key values can be declared as concise methods instead
  def detectMethods(n: AST_Node): AST_Node = {
    n.transformBefore {(node, descend, transformer) =>
      node match {
        case obj@AST_Object(props) =>
        //case AST_Definitions(AST_VarDef(AST_SymbolDef(df), Defined(obj@AST_Object(props)))) =>
          // check which members are ever written to - we can convert all others to getters and methods
          transformer.parent(1).nonNull match {
            case Some(AST_Definitions(AST_VarDef(AST_SymbolDef(df), Defined(o: AST_Object)))) =>
              //println(s"Scan object ${df.name} for methods")
              assert(o == obj)


              object IsDf extends Extractor[String] {
                def unapply(arg: AST_Node) = arg match {
                  case AST_SymbolRefDef(`df`) AST_Dot key => Some(key)
                  case _ => None
                }
              }
              object IsDfModified extends IsModified(IsDf)

              var modifiedMembers = Set.empty[String]
              walkReferences(df, IsDfModified) { key =>
                modifiedMembers += key
                false
              }

              //println(s"Detected modified members $modifiedMembers")

              val newProps = props.map {
                case kv@AST_ObjectKeyVal(k, f@AST_Function(args, body)) =>
                  if (modifiedMembers contains k) {
                    //println(s"Modified member $k")
                    kv
                  } else new AST_ConciseMethod {
                    fillTokens(this, kv)
                    key = new AST_SymbolMethod {
                      fillTokens(this, f)
                      name = k
                      // thedef - nowhere to get it?
                    }
                    value = new AST_Accessor {
                      fillTokens(this, f)
                      argnames = args
                      this.body = body.toJSArray
                    }
                  }

                case p => p
              }
              val newObj = obj.clone()
              newObj.properties = newProps.toJSArray
              newObj

            case None =>
              node
            case Some(p) =>
              //println(s"Parent ${nodeClassName(p)}")
              node
          }
        case _ =>
          val n = node.clone()
          descend(n, transformer)
          n
      }
    }
  }

  def convertConstToFunction(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case AST_Const(AST_VarDef(sym, Defined(AST_Function(args, body)))) =>
          new AST_Defun {
            defun =>
            name = new AST_SymbolDefun {
              name = sym.name
              thedef = sym.thedef
              scope = sym.scope
              init = js.Array[AST_Node](defun)
            }
            fillTokens(this, node)
            argnames = args
            this.body = body.toJSArray
          }
        case _ => node
      }
    }
  }

  // merge variable declaration and first assignment if possible
  def varInitialization(n: AST_Node): AST_Node = {

    // walk the tree, check for first reference of each var
    var pairs = Map.empty[SymbolDef, AST_SymbolRef]
    n.walk { node =>
      node match {
        case AST_VarDef(name, value) if value.nonNull.isEmpty =>
          //println(s"AST_VarDef ${name.name}")
          for (df <- name.thedef) {
            assert(df.name == name.name)
            if (df.references.nonEmpty) {
              // find the first reference
              val firstRef = df.references.minBy { ref =>
                assert(ref.thedef contains df)
                ref.start.map(_.pos).getOrElse(Int.MaxValue)
              }
              // if the first ref is in the current scope, we might merge it with the declaration
              if (firstRef.scope == name.scope) {
                pairs += df -> firstRef
              }

            }
          }
        case _ =>
      }
      false
    }

    val refs = pairs.values.toSet
    var replaced = Set.empty[SymbolDef]

    //println(s"transform, vars ${pairs.keys.map(_.name).mkString(",")}")

    // walk the tree, check for possible val replacements and perform them
    val changeAssignToVar = n.transformAfter {(node, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      //println(s"node ${nodeClassName(node)} ${ScalaOut.outputNode(node, "")}")
      // we need to descend into assignment definitions, as they may contain other assignments
      node match {
        case AST_SimpleStatement(AST_Assign(sr@AST_SymbolRef(name, scope, thedef), "=", right)) if refs contains sr =>
          //println(s"ss match assign ${nodeClassName(left)} ${ScalaOut.outputNode(left, "")}")
          val stackTail = transformer.stack.takeRight(2).dropRight(1).toSeq
          stackTail match {
            case Seq(_: AST_Block) =>
              val vr = new AST_Var
              val vv = new AST_VarDef
              vr.definitions = js.Array(vv)
              vv.name = new AST_SymbolVar
              vv.name.thedef = thedef
              vv.name.name = name
              vv.name.init = js.Array(right)
              vv.value = right
              vv.name.scope = scope
              for (d <- thedef; orig <- d.orig.headOption)
              {
                fillTokens(vr, orig)
                fillTokens(vv, orig)
                fillTokens(vv.name, orig)
              }
              //println(s"Replaced ${vv.name.name} AST_SymbolRef with AST_VarDef, value ${nodeTreeToString(right)}")
              replaced ++= thedef.nonNull
              vr
            case _ =>
              node
          }
        case _ =>
          node
      }
    }

    //println(s"transform done, replaced ${replaced.map(_.name).mkString(",")}")

    pairs = pairs.filterKeys(replaced.contains)

    // walk the tree, check for possible val replacements and perform them
    changeAssignToVar.transformAfter{ (node, _) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      node match {
        case v: AST_Var =>
          // remove only the original declaration, not the one introduced by us
          // original declaration has no init value
          val af = v.definitions.filterNot { d =>
            d.value.nonNull.isEmpty &&
              d.name.thedef.exists(pairs.contains)
          }
          val vv = v.clone()
          vv.definitions = af
          vv
        case c =>
          c
      }
    }
  }


}
