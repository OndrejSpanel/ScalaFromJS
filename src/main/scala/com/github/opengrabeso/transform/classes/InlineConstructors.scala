package com.github.opengrabeso
package transform
package classes

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Expressions._
import TransformClasses._
import Variables._
import Symbols._
import VariableUtils._

import scala.scalajs.js
import js.JSConverters._

object InlineConstructors {
  private case class PrivateMember(sym: SymbolDef, isVal: Boolean)

  private def detectPrivateMembers(n: AST_Lambda): Seq[PrivateMember] = {
    // any variable defined in the main body scope and references from any function is considered a private member
    //n.variables = Dictionary.empty[SymbolDef]

    val refs = buildReferenceStacks(n)

    // TODO: consider only references from exported functions, ignore references from local named functions
    // beware of local functions used from exported functions, including their transitive closure
    // to stay safe, we do not ignore them now (may result in more private members than needed)
    //println(s"n.variables ${n.variables.map(_._1)}")
    //println(s"n.functions ${n.functions.map(_._1)}")
    val variables = n.variables.filter(v => !n.functions.contains(v._1))
    val privates = for {
      (_, sym) <- variables
      // empty 'references' means automatic symbol, like "arguments"
      rs <- refs.refs.get(sym)
      if (rs -- Set(n)).exists(_.isInstanceOf[AST_Lambda])
    } yield {
      sym
    }

    privates.toSeq.map { priv =>

      //println(s"  priv ${priv.name} ${refs.refs.get(priv)} ")
      // check which members are ever written to - we can convert all others to getters and methods
      val modified = refs.isModified(priv)

      PrivateMember(priv, !modified)

    }
  }

  def privateVariables(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          for {
            constructorProperty@AST_ConciseMethod(_, rawConstructor: AST_Lambda) <- findConstructor(cls)
          } {
            val locals = detectPrivateMembers(rawConstructor)
            //println(s"Locals ${locals.map(l => l.sym.name -> l.isVal)}")
            // convert private variables to members (TODO: mark them as private somehow)

            def newThisDotMember(member: String) = new AST_Dot {
              expression = new AST_This {
                name = "this"
              }
              property = member
            }

            def privateMember(v: SymbolDef): AST_Node = newThisDotMember(v.name)

            val constructor = locals.foldLeft(rawConstructor) { (constructor, privateVar) =>
              val replacedInit = replaceVariableInit(constructor, privateVar.sym) { (sym, init) =>
                new AST_SimpleStatement {
                  body = new AST_Assign {
                    left = newThisDotMember(sym.name)
                    operator = "="
                    right = init.clone()
                  }
                }
              }
              replaceVariable(replacedInit, privateVar.sym, privateMember(privateVar.sym))
            }
            constructorProperty.value = constructor

            // DRY: FillVarMembers
            val clsTokenDef = classTokenSource(cls)

            val vars = locals.map { local =>
              val varDecl = if (local.isVal) new AST_Const else new AST_Var
              fillTokens(varDecl, clsTokenDef)
              varDecl.definitions = js.Array(AST_VarDef.uninitializedSym(clsTokenDef)(local.sym))
              //println(s"privateVariables ${local.sym.name} $varDecl ${cls.start.get.pos}")
              varDecl
            }

            if (vars.nonEmpty) {
              val accessor = classInlineBody(cls, clsTokenDef)

              accessor.body ++= (vars: Iterable[AST_Statement]).toJSArray
              //println(s"privateVariables newMembers $vars")

              // remove overwritten members
              //cls.properties = cls.properties.filterNot(p => newMembers.contains(propertyName(p)))
            }

          }
          cls
        case _ =>
          node
      }
    }
  }

  def privateFunctions(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          for {
            constructorProperty@AST_ConciseMethod(_, rawConstructor: AST_Lambda) <- findConstructor(cls)
          } {
            object DefinePrivateFunction {
              def unapply(arg: AST_Statement) = arg match {
                case SingleStatement(AST_Assign(AST_This() AST_Dot funName, "=", lambda: AST_Lambda)) =>
                  Some(funName, lambda)
                case _ =>
                  None
              }
            }
            val (functions, rest) = rawConstructor.body.partition {
              case DefinePrivateFunction(_, _) =>
                true
              case _ =>
                false
            }

            constructorProperty.value._body = rest
            // add functions as methods
            cls.properties = cls.properties ++ functions.map {
              case node@DefinePrivateFunction(funName, lambda) =>
                new AST_ConciseMethod {
                  fillTokens(this, node)
                  key = new AST_SymbolMethod {
                    /*_*/
                    fillTokens(this, node)
                    /*_*/
                    name = funName
                    // scope, thedef will be filled

                  }
                  value = new AST_Accessor {
                    fillTokens(this, node)
                    name = lambda.name
                    argnames = lambda.argnames
                    uses_arguments = lambda.uses_arguments
                    this.body = lambda.body
                  }
                  `static` = false
                  is_generator = false
                  quote = "\""
                }

            }
          }
          cls
        case _ =>
          node
      }
    }
  }

  def apply(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          val clsTokenDef = classTokenSource(cls)
          for {
            constructorProperty@AST_ConciseMethod(_, constructor: AST_Lambda) <- findConstructor(cls)
          } {
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
