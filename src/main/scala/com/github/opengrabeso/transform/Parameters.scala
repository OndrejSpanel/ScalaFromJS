package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import scalajs.js
import Transform._

object Parameters {

  /**
    * Scan all parameters in all functions, parameters are scaned from last to first (right to left)
    * @param process process the function - once this returns None, scan is aborted
    * */
  def processAllFunctions(n: AST_Node, process: (AST_Lambda, AST_SymbolFunarg) => Option[AST_Lambda]): AST_Node = {

    def processOneFunction(f: AST_Lambda): AST_Lambda = {

      def processArguments(f: AST_Lambda, args: Seq[AST_SymbolFunarg]): AST_Lambda = args match {
        case Seq() =>
          f
        case head +: tail =>
          process(f, head).fold(f) {
            processArguments(_, tail)
          }
      }

      processArguments(f, f.argnames.toSeq.reverse)
    }

    n.transformAfter { (node, _) =>
      node match {
        case f: AST_Defun =>
          processOneFunction(f)
        case m: AST_ConciseMethod =>
          //println(s"introduceDefaultValues ${m.key.name}")
          m.value = processOneFunction(m.value)
          m
        case _ =>
          node
      }
    }

    n
  }

  class CompareWithUndefined(parName: String) {
    def unapply(arg: AST_Node) = arg match {
      case AST_Binary(AST_SymbolRefName(`parName`), op, AST_SymbolRefName("undefined")) =>
        Some(op)
      case _ =>
        None
    }
  }

  def defaultValues(n: AST_Node): AST_Node = {

    object InitStatement {
      def unapply(arg: AST_Node) = arg match {
        case c: AST_Constant => Some(c)
        // TODO: accept only some forms of new or Array (avoid reordering dependent expressions)
        case c: AST_Array => Some(c)
        case c: AST_New => Some(c)
        // TODO: check for dependent expressions
        case c: AST_SymbolRef => Some(c)
        case c@((x: AST_SymbolRef) AST_Dot _) => Some(c)
        case _ =>
          //println(s"${nodeClassName(arg)}")
          None
      }
    }

    // the only use of a parameter is in a `x_par || value` form
    def introduceDefaultValue(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      val parName = par.name
      //println(s"introduceDefaultValue $parName")

      val defByOr = {
        // if there is only one reference, check if it is handling the default value
        object IsParDefaultHandling {

          object CompareParWithUndefined extends CompareWithUndefined(parName)

          def unapply(arg: AST_Node) = arg match {
            // TODO: allow other initialization expressions, not only AST_Constant
            case AST_Binary(symRef@AST_SymbolRefName(`parName`), "||", InitStatement(init)) =>
              Some(symRef, init)
            case AST_Conditional(CompareParWithUndefined("!=" | "!=="), symRef@AST_SymbolRefName(`parName`), InitStatement(init)) =>
              Some(symRef, init)
            case AST_Conditional(CompareParWithUndefined("==" | "==="), InitStatement(init), symRef@AST_SymbolRefName(`parName`)) =>
              Some(symRef, init)
            case _ =>
              None
          }
        }

        var defValue = Option.empty[AST_Node]
        var otherUse = false
        f.walk {
          case IsParDefaultHandling(_, init) =>
            //println(s"Detected def value for $parName")
            if (!otherUse) defValue = Some(init)
            true // use inside of the def. value pattern must not set otherUse
          case AST_SymbolRefName(`parName`) =>
            otherUse = true
            defValue = None
            otherUse
          case _ =>
            otherUse
        }
        defValue.map { init =>
          par.init = js.Array(init)
          // remove the use
          f.transformAfter { (node, _) =>
            node match {
              case IsParDefaultHandling(symRef, _) =>
                symRef
              case _ =>
                node
            }
          }
        }

      }

      defByOr.orElse {
        object IsParDefaultHandlingByIf {
          def unapply(arg: AST_Node) = arg match {
            case AST_If(
            AST_Binary(symRef@AST_SymbolRefName(`parName`), "==" | "===", AST_SymbolRefUndefined()),
            SingleStatement(AST_Assign(AST_SymbolRefName(`parName`), "=", InitStatement(init))),
            None
            ) =>
              //println(s"IsParDefaultHandling of $parName via if")
              Some(symRef, init)
            case _ =>
              //println(s"no IsParDefaultHandling in ${nodeClassName(arg)}")
              None
          }
        }

        var defValue = Option.empty[AST_Node]
        var alreadyUsed = false
        f.walk {
          case IsParDefaultHandlingByIf(_, init) =>
            //println(s"Detected def value for $parName")
            if (!alreadyUsed) defValue = Some(init)
            alreadyUsed = true
            true // use inside of the def. value pattern must not set otherUse
          case AST_SymbolRefName(`parName`) =>
            alreadyUsed = true
            alreadyUsed
          case _ =>
            alreadyUsed
        }
        defValue.map { init =>
          par.init = js.Array(init)
          // remove the use
          //println("Removed default par")
          f.transformAfter { (node, _) =>
            node match {
              case s@IsParDefaultHandlingByIf(_, _) =>
                new AST_EmptyStatement {
                  fillTokens(this, s)
                }
              case _ =>
                node
            }
          }
        }
      }
    }


    processAllFunctions(n, introduceDefaultValue)

  }

  def modifications(n: AST_Node): AST_Node = {
    import VariableUtils._


    def handleModification(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      par.thedef.nonNull.map { parDef =>
        val parName = par.name
        //println(s"Checking $parName")

        // check if the parameter is ever modified
        val refs = buildReferenceStacks(n)

        object IsPar extends Extractor[Unit] {
          def unapply(arg: AST_Node) = arg match {
            case AST_SymbolRefDef(`parDef`) => Some(())
            case _ => None
          }
        }
        object IsParModified extends IsModified(IsPar)

        val assignedInto = refs.walkReferences(parDef, IsParModified)(_ => true)
        if (assignedInto) {
          //println(s"Detected assignment into $parName")
          // we need to replace parameter x with x_par and intruduce var x = x_par
          val newF = f.clone()

          val parIndex = f.argnames.indexOf(par)
          val parNode = par.clone()

          //val oldParSym = parNode.thedef

          // introduce a new symbol
          parNode.thedef = js.undefined
          parNode.name = parName + SymbolTypes.parSuffix

          newF.argnames(parIndex) = parNode

          newF.body = js.Array(
            new AST_Let {
              fillTokens(this, parNode)
              definitions = js.Array(new AST_VarDef {
                fillTokens(this, parNode)
                name = new AST_SymbolVar {
                  fillTokens(this, parNode)
                  name = parName
                }
                value = new AST_SymbolRef {
                  /*_*/
                  fillTokens(this, parNode)
                  /*_*/
                  name = parName + SymbolTypes.parSuffix
                }
              })
            }
          ) ++ f.body

          newF

        } else {
          f
        }
      }


    }

    processAllFunctions(n, handleModification)
  }

  def removeDeprecated(n: AST_Node): AST_Node = {
    def removeOneDeprecated(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      val parName = par.name
      //println(s"removeOneDeprecated $parName")

      def containsDeprecation(body: Seq[AST_Statement]) = {
        body.exists {
          case AST_SimpleStatement(AST_Call(AST_SymbolRefName("console") AST_Dot "warn", _)) =>
            true
          case _: AST_Throw =>
            true
          case _ =>
            false
        }
      }

      object IsParDeprecated {

        object CompareParWithUndefined extends CompareWithUndefined(parName)

        def unapply(arg: AST_Node) = arg match {
          case AST_If(CompareParWithUndefined("!=" | "!=="), Statements(body), None) if containsDeprecation(body) =>
            //println("IsParDeprecated")
            true
          case _ =>
            //println(s"no IsParDeprecated in ${nodeClassName(arg)}")
            false
        }
      }

      var isDeprecated = false
      var otherUse = false
      f.walk {
        case IsParDeprecated() =>
          //println(s"Detected deprecated par for $parName")
          isDeprecated = true
          true // use inside of the current pattern must not set otherUse
        case AST_SymbolRefName(`parName`) =>
          otherUse = true
          true
        case _ =>
          otherUse

      }

      if (isDeprecated && !otherUse) Some {
        f.argnames = f.argnames diff Seq(par)
        f.transformAfter { (node, _) =>
          node match {
            case s@IsParDeprecated() =>
              new AST_EmptyStatement {
                fillTokens(this, s)
              }
            case _ =>
              node
          }
        }
      } else None
    }

    processAllFunctions(n, removeOneDeprecated)

  }

  def inlineConstructorVars(n: AST_Extended): AST_Extended = {
    var types = n.types
    def handleConstructorVars(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      // inline all parameters, or constructor only?
      val parName = par.name
      if (parName.endsWith(SymbolTypes.parSuffix) && par.thedef.isDefined){
        val parDef = par.thedef.get
        object IsVarPar {

          def unapply(arg: AST_Node) = arg match {
            case AST_Var(AST_VarDef(AST_Symbol(symName, _, Defined(symDef)), Defined(AST_SymbolRef(_, _, Defined(`parDef`))))) =>
              //println(s"IsVarPar $symName ${parDef.name}")
              Some(symDef)
            case _ =>
              //println(s"no IsParDeprecated in ${nodeClassName(arg)}")
              None
          }
        }

        var isVarPar = Option.empty[SymbolDef]
        var otherUse = false
        f.walk {
          case IsVarPar(varSym) =>
            //println(s"Detected deprecated par for $parName")
            if (!otherUse) isVarPar = Some(varSym)
            true // use inside of the current pattern must not set otherUse
          case AST_SymbolRefName(`parName`) =>
            isVarPar = None
            otherUse = true
            true
          case _ =>
            otherUse

        }

        isVarPar.map { varSym =>
          val parIndex = f.argnames.indexOf(par)
          val parNode = f.argnames(parIndex).clone()

          val oldParSym = parNode.thedef

          // change varSym declaration location, so that type information works
          varSym.orig = js.Array(parNode)

          parNode.thedef = varSym
          parNode.name = varSym.name

          f.argnames(parIndex) = parNode

          // redirect also a symbol type
          for {
            parSym <- oldParSym
            tpe <- types.get(parSym)
          } {
            //println(s"${parSym.name} -> ${varSym.name} Redirect type $tpe")
            types += SymbolTypes.id(varSym) -> tpe
          }


          f.transformAfter { (node, _) =>
            node match {
              case IsVarPar(`varSym`) =>
                new AST_EmptyStatement {
                  fillTokens(this, node)
                }
              case _ =>
                node
            }
          }
        }.orElse(Some(f))

      } else Some(f)
    }

    val processed = processAllFunctions(n.top, handleConstructorVars)
    //println(s"Before: ${n.types}")
    //println(s"After: $types")
    AST_Extended(processed.asInstanceOf[AST_Toplevel], types)

  }
}
