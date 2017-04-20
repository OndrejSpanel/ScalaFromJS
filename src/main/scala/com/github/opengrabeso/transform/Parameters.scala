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
        case c@AST_Object(Seq()) => Some(c)
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

      // if there is only one reference, check if it is handling the default value
      object CompareParWithUndefined extends CompareWithUndefined(parName)

      object CheckParIsUndefined {
        def unapply(arg: AST_Node): Boolean = arg match {
          // par == undefined
          case CompareParWithUndefined("==" | "===") =>
            true
          // !par
          case AST_UnaryPrefix("!", AST_SymbolRefName(`parName`)) =>
            true
          case _ =>
            false
        }
      }

      object CheckParNotUndefined {
        def unapply(arg: AST_Node): Boolean = arg match {
          // par != undefined
          case CompareParWithUndefined("!=" | "!==") =>
            true
          // par
          case AST_SymbolRefName(`parName`) =>
            true
          case _ =>
            false
        }
      }

      object IsParDefaultHandling {
        def unapply(arg: AST_Node) = arg match {
          case AST_Binary(symRef@AST_SymbolRefName(`parName`), "||", InitStatement(init)) =>
            Some(symRef, init)
          case AST_Conditional(CheckParNotUndefined(), symRef@AST_SymbolRefName(`parName`), InitStatement(init)) =>
            Some(symRef, init)
          case AST_Conditional(CheckParIsUndefined(), InitStatement(init), symRef@AST_SymbolRefName(`parName`)) =>
            Some(symRef, init)
          case _ =>
            None
        }
      }

      object IsParDefaultHandlingAssignment {
        def unapply(arg: AST_Node) = arg match {
          case AST_SimpleStatement(AST_Assign(AST_SymbolRefName(`parName`), "=", IsParDefaultHandling(_, init))) =>
            Some(init)

          case AST_If(CheckParIsUndefined(), SingleStatement(AST_Assign(AST_SymbolRefName(`parName`), "=", init)), None) =>
            Some(init)

          case _ => None
        }
      }

      var defValueCond = Option.empty[AST_Node]
      var defValueAssign = Option.empty[AST_Node]
      var otherUse = false
      var alreadyUsed = false
      f.walk {
        case IsParDefaultHandling(_, init) =>
          //println(s"Detected def value for $parName")
          if (!otherUse) defValueCond = Some(init)
          otherUse = true
          true // use inside of the def. value pattern must not set otherUse
        case IsParDefaultHandlingAssignment(init) =>
          //println(s"Detected def value assignment for $parName")
          if (!alreadyUsed) defValueAssign = Some(init)
          alreadyUsed = true
          true // use inside of the def. value pattern must not set otherUse
        case AST_SymbolRefName(`parName`) =>
          otherUse = true
          alreadyUsed = true
          defValueCond = None
          false
        case _ =>
          false
      }

      defValueCond.toSeq ++ defValueAssign match { // use only one option - two are bad
        case Seq(init) =>
          par.init = js.Array(init)
          // remove the use
          Some(f.transformBefore { (node, descend, transform) =>
            node match {
              case IsParDefaultHandlingAssignment(_) if defValueAssign.isDefined =>
                AST_EmptyStatement(node)
              case IsParDefaultHandling(symRef, _) if defValueCond.isDefined =>
                symRef.clone()
              case _ =>
                descend(node.clone(), transform)
            }
          })
        case _ =>
          None
      }
    }


    processAllFunctions(n, introduceDefaultValue)

  }

  def modifications(n: AST_Node): AST_Node = {
    import VariableUtils._

    val refs = buildReferenceStacks(n)

    def handleModification(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      par.thedef.nonNull.map { parDef =>
        val parName = par.name
        //println(s"Checking $parName")

        // check if the parameter is ever modified

        object IsPar extends Extractor[Unit] {
          def unapply(arg: AST_Node) = arg match {
            case AST_SymbolRefDef(`parDef`) => Some(())
            case _ => None
          }
        }
        object IsParModified extends IsModified(IsPar)

        // TODO: cloning destroys reference stacks - gather first

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
          parNode.name = parName + Symbols.parSuffix

          newF.argnames(parIndex) = parNode

          newF.body = js.Array(
            new AST_Let {
              fillTokens(this, parNode)
              definitions = js.Array(AST_VarDef(parNode) (
                new AST_SymbolVar {
                  fillTokens(this, parNode)
                  name = parName
                },
                new AST_SymbolRef {
                  /*_*/
                  fillTokens(this, parNode)
                  /*_*/
                  name = parName + Symbols.parSuffix
                }
              ))
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
              AST_EmptyStatement(s)
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
      if (!f.name.nonNull.exists(_.name == Classes.inlineBodyName)) Some(f)
      else {
        // inline all parameters, or constructor only?
        val parName = par.name
        if (parName.endsWith(Symbols.parSuffix) && par.thedef.isDefined) {
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
            //println(s"Rename ${varSym.name} in ${f.name.get.name}")

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
                  AST_EmptyStatement(node)
                case _ =>
                  node
              }
            }
          }.orElse(Some(f))

        } else Some(f)
      }
    }

    val processed = processAllFunctions(n.top, handleConstructorVars)
    //println(s"Before: ${n.types}")
    //println(s"After: $types")
    n.copy(top = processed.asInstanceOf[AST_Toplevel], types = types)

  }
}
