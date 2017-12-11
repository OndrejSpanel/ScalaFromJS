package com.github.opengrabeso
package transform

import JsUtils._
import net.gamatron.esprima._
import esprima._

import Transform._
import Symbols._
import Expressions._

object Parameters {

  /*
  * AST types are incorrect - function parameters may be of a type other than Node.SymbolFunarg
  * This allows us to hotfix this.
  * */
  def isNormalPar(par: Node.Node): Boolean = {
    //
    par match {
      case _: Node.Destructuring =>
        false
      case _: Node.DefaultAssign =>
        false
      case _: Node.SymbolFunarg =>
        true
      case _ =>
        false
    }
  }

  /**
    * Scan all parameters in all functions, parameters are scaned from last to first (right to left)
    * @param process process the function - once this returns None, scan is aborted
    * */

  def processAllFunctions(n: Node.Node, process: (Node.Lambda, Node.SymbolFunarg) => Option[Node.Lambda]): Node.Node = {

    def processOneFunction(f: Node.Lambda): Node.Lambda = {

      def processArguments(f: Node.Lambda, args: Seq[Node.SymbolFunarg]): Node.Lambda = args match {
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
        case f: Node.Defun =>
          processOneFunction(f)
        case m: Node.ConciseMethod =>
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
    def unapply(arg: Node.Node) = arg match {
      case Node.BinaryExpression(Node.Identifier(`parName`), op, Node.Identifier("undefined")) =>
        Some(op)
      case _ =>
        None
    }
  }

  def defaultValues(n: Node.Node): Node.Node = {

    // the only use of a parameter is in a `x_par || value` form
    def introduceDefaultValue(f: Node.Lambda, par: Node.SymbolFunarg): Option[Node.Lambda] = {
      if (!isNormalPar(par)) return None
      val parName = par.name
      //println(s"introduceDefaultValue $parName")

      // if there is only one reference, check if it is handling the default value
      object CompareParWithUndefined extends CompareWithUndefined(parName)

      object CheckParIsUndefined {
        def unapply(arg: Node.Node): Boolean = arg match {
          // par == undefined
          case CompareParWithUndefined("==" | "===") =>
            true
          // !par
          case Node.UnaryPrefix("!", Node.Identifier(`parName`)) =>
            true
          case _ =>
            false
        }
      }

      object CheckParNotUndefined {
        def unapply(arg: Node.Node): Boolean = arg match {
          // par != undefined
          case CompareParWithUndefined("!=" | "!==") =>
            true
          // par
          case Node.Identifier(`parName`) =>
            true
          case _ =>
            false
        }
      }

      object IsParDefaultHandling {
        def unapply(arg: Node.Node) = arg match {
          case Node.BinaryExpression(symRef@Node.Identifier(`parName`), "||", InitStatement(init)) =>
            Some(symRef, init)
          case Node.Conditional(CheckParNotUndefined(), symRef@Node.Identifier(`parName`), InitStatement(init)) =>
            Some(symRef, init)
          case Node.Conditional(CheckParIsUndefined(), InitStatement(init), symRef@Node.Identifier(`parName`)) =>
            Some(symRef, init)
          case _ =>
            None
        }
      }

      object IsParDefaultHandlingAssignment {
        def unapply(arg: Node.Node) = arg match {
          case Node.SimpleStatement(Node.Assign(Node.Identifier(`parName`), "=", IsParDefaultHandling(_, init))) =>
            Some(init)

          case Node.IfStatement(CheckParIsUndefined(), SingleStatement(Node.Assign(Node.Identifier(`parName`), "=", init)), None) =>
            Some(init)

          case _ => None
        }
      }

      var defValueCond = Option.empty[Node.Node]
      var defValueAssign = Option.empty[Node.Node]
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
        case Node.Identifier(`parName`) =>
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
                Node.EmptyStatement(node)
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

  private def renameParameter(f: Node.Lambda, par: Node.SymbolFunarg, newName: String) = {
    val parIndex = f.argnames.indexOf(par)
    val parNode = par.clone()

    // introduce a new symbol
    parNode.thedef = js.undefined
    parNode.name = newName

    f.argnames(parIndex) = parNode

    parNode
  }


  def modifications(n: Node.Node): Node.Node = {
    import VariableUtils._

    val refs = buildReferenceStacks(n)

    def handleModification(f: Node.Lambda, par: Node.SymbolFunarg): Option[Node.Lambda] = {
      par.thedef.nonNull.map { parDef =>
        val parName = par.name
        //println(s"Checking $parName")


        // TODO: cloning destroys reference stacks - gather first

        // check if the parameter is ever modified
        val assignedInto = refs.isModified(parDef)
        if (assignedInto) {
          //println(s"Detected assignment into $parName")
          // we need to replace parameter x with x_par and intruduce var x = x_par
          val newF = f.clone()

          val parNode = renameParameter(newF, par, parName + Symbols.parSuffix)

          newF.body = js.Array(
            Node.Let(parNode)(Node.VarDef.initialized(parNode)(parName, Node.Identifier(parNode)(parName + Symbols.parSuffix)))
          ) ++ f.body

          newF

        } else {
          f
        }
      }


    }

    processAllFunctions(n, handleModification)
  }

  /*
  If a class inline body parameter is named xxx_par and a member named xxx does not exist, we can remove the _par
  */
  def simpleParameters(n: NodeExtended): NodeExtended = {

    var types = n.types

    def handleSimpleParameters(f: Node.Lambda, par: Node.SymbolFunarg): Option[Node.Lambda] = {

      if (!f.name.nonNull.exists(_.name == Classes.inlineBodyName)) Some(f)
      else {
        par.thedef.nonNull.map { parDef =>
          val parName = par.name

          if (parName endsWith parSuffix) {
            // check for existence of variable without a suffix
            val shortName = parName dropRight parSuffix.length
            val conflict = f.body.exists {
              case Node.Definitions(Node.VarDef(Node.SymbolName(`shortName`), _)) =>
                true
              case _ =>
                false
            }
            if (!conflict) {
              // we may rename the variable now
              val renamedBody = f.body.map { s =>
                //println(s"Renaming $s")
                Variables.renameVariable(s, parDef, shortName)
              }
              f._body = renamedBody

              renameParameter(f, par, shortName)
              // rename hints as well
              for (parId <- symbolId(par)) {
                types = types.renameHint(parId, parId.copy(name = shortName))
              }

              f
            }
            else f

          } else f
        }
      }

    }

    val ret = processAllFunctions(n.top, handleSimpleParameters).asInstanceOf[Node.Program]
    n.copy(top = ret, types = types)
  }


  def removeDeprecated(n: Node.Node): Node.Node = {
    def removeOneDeprecated(f: Node.Lambda, par: Node.SymbolFunarg): Option[Node.Lambda] = {
      if (!isNormalPar(par)) return None
      val parName = par.name

      def containsDeprecation(body: Seq[Node.Statement]) = {
        body.exists {
          case Node.SimpleStatement(Node.Call(Node.Identifier("console") Node.StaticMemberExpression "warn", _)) =>
            true
          case _: Node.Throw =>
            true
          case _ =>
            false
        }
      }

      object IsParDeprecated {

        object CompareParWithUndefined extends CompareWithUndefined(parName)

        def unapply(arg: Node.Node) = arg match {
          case Node.IfStatement(CompareParWithUndefined("!=" | "!=="), Statements(body), None) if containsDeprecation(body) =>
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
        case Node.Identifier(`parName`) =>
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
              Node.EmptyStatement(s)
            case _ =>
              node
          }
        }
      } else None
    }

    processAllFunctions(n, removeOneDeprecated)

  }

  /*
  find constructor parameters which are assigned to a var member and replace them with a var parameter
  * */
  def inlineConstructorVars(n: NodeExtended): NodeExtended = {
    var types = n.types
    val logging = false
    def handleConstructorVars(f: Node.Lambda, par: Node.SymbolFunarg): Option[Node.Lambda] = {
      if (!f.name.nonNull.exists(_.name == Classes.inlineBodyName)) Some(f)
      else {
        // inline all parameters, or constructor only?
        // check hints: is it a variable?

        val isPurePar = n.types.getHint(symbolId(par)).contains(IsConstructorParameter)

        val parName = par.name
        if (logging) println(s"Checking par $parName in ${f.name.map(_.name)}")
        if (isPurePar) {
          if (logging) println(s"  is a pure parameter")
          val parDef = par.thedef.get
          object IsVarPar {

            def unapply(arg: Node.Node) = arg match {
              case Node.Var(Node.VarDef(Node.Identifier(symName, _, Defined(symDef)), Defined(Node.Identifier(_, _, Defined(`parDef`))))) =>
                //println(s"IsVarPar $symName ${parDef.name}")
                Some(symDef)
              case _ =>
                //println(s"no IsParDeprecated in ${nodeClassName(arg)}")
                None
            }
          }

          def isParRef(n: Node.Node) = {
            n match {
              case Node.Identifier(`parName`) =>
                true
              case _ =>
                false
            }

          }

          var isVarPar = Option.empty[SymbolDef]
          var otherUse = false
          f.walk {
            case IsVarPar(varSym) =>
              if (logging) println(s"Detected variable par ${varSym.name} for $parName")
              // only one var-par is allowed
              if (!otherUse && isVarPar.isEmpty) isVarPar = Some(varSym)
              else {
                if (logging) println(s"  multiple variable par candidates for $parName")
                isVarPar = None
                otherUse = true
              }
              true // use inside of the current pattern must not set otherUse
            case Node.Call(Node.This() Node.StaticMemberExpression "constructor", args@_*) if args.exists(isParRef) =>
              // passed to the constructor - this is always allowed
              // should we check what the constructor does with the value?
              //println(s"Detected constructor call for $parName")
              true
            case Node.Identifier(`parName`) =>
              if (logging) println(s"Detected other use for $parName")
              isVarPar = None
              otherUse = true
              true
            case _ =>
              otherUse

          }

          isVarPar.map { varSym =>
            if (logging) println(s"Rename ${varSym.name} in ${f.name.get.name}")

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
                  Node.EmptyStatement(node)
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
    n.copy(top = processed.asInstanceOf[Node.Program], types = types)

  }
}
