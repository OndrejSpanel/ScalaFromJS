package com.github.opengrabeso
package transform
package classes

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Expressions._
import TransformClasses._
import Symbols._

object InlineConstructors {
  private def detectPrivateMembers(n: AST_Lambda): Seq[AST_SymbolVar] = {
    // any variable defined in the main body scope and references from any function is considered a private member
    //n.variables = Dictionary.empty[SymbolDef]
    val locals = for {
      (_, sym) <- n.variables
      if sym.thedef.exists(_.references.nonEmpty) // empty 'references' means automatic symbol, like "arguments"
    } yield {
      sym
    }

    /*
    n.walk {
      case AST_Definitions() =>
        false
      case _ =>
        true
    }
    */
    locals.toSeq
  }

  def apply(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          val clsTokenDef = classTokenSource(cls)
          for {
            constructorProperty@AST_ConciseMethod(_, constructor: AST_Lambda) <- findConstructor(cls)
          } {
            val locals = detectPrivateMembers(constructor)
            println(s"Locals ${locals.map(_.name)}")
            // anything before a first variable declaration can be inlined, variables need to stay private
            val (inlined, rest_?) = constructor.body.span {
              case _: AST_Definitions => false
              case _ => true
            }

            val (inlineVars, rest) = rest_?.partition {
              case SingleStatement(AST_Assign((_: AST_This) AST_Dot member, "=", IsConstantInitializer(expr))) =>
                //println(s"Assign const $expr")
                true
              case _ =>
                false
            }
            //println(s"inlined ${inlined.map(nodeClassName)}")
            //println(s"rest ${rest.map(nodeClassName)}")
            // transform parameter names while inlining (we need to use parSuffix names)
            val parNames = constructor.argnames.map(_.name)
            val parNamesSet = parNames.toSet
            object IsParameter {
              def unapply(arg: String): Boolean = parNamesSet contains arg
            }
            val parNamesAdjusted = (inlined ++ inlineVars).map { s =>
              s.transformAfter { (node, transformer) =>
                node match {
                  case sym@AST_SymbolName(IsParameter()) =>
                    sym.name = sym.name + parSuffix
                    sym
                  // do not inline call, we need this.call form for the inference
                  // on the other hand form without this is better for variable initialization
                  case (_: AST_This) AST_Dot member if !transformer.parent().isInstanceOf[AST_Call] =>
                    AST_SymbolRef(clsTokenDef)(member)
                  case _ =>
                    node
                }
              }
            }
            // add adjusted constructor argument names so that parser correctly resolves them inside of the function
            val accessor = classInlineBody(cls, clsTokenDef)
            accessor.argnames = constructor.argnames.map { p =>
              val a = p.clone()
              a.name = p.name + parSuffix
              // marking source as cls so that they use the class scope, same as member variables
              fillTokens(a, cls)
              a
            }
            //println(s"inlineConstructors classInlineBody clone ${accessor.argnames}")


            // add the constructor call itself, so that type inference binds its parameters and arguments
            val constructorCall = if (rest.nonEmpty) Some(AST_SimpleStatement(constructorProperty) {
              new AST_Call {
                fillTokens(this, constructorProperty)
                expression = new AST_Dot {
                  fillTokens(this, constructorProperty)
                  expression = new AST_This() {
                    fillTokens(this, constructorProperty)
                  }
                  property = "constructor"
                }

                args = constructor.argnames.map { p =>
                  AST_SymbolRef(p)(p.name + parSuffix)
                }
              }
            }) else None

            accessor.body = accessor.body ++ parNamesAdjusted ++ constructorCall

            if (rest.nonEmpty) {
              constructor.body = rest
            } else {
              cls.properties = cls.properties -- Seq(constructorProperty)
            }
          }

          cls
        case _ =>
          node
      }
    }

    // detect constructors
    // detect
  }

}
