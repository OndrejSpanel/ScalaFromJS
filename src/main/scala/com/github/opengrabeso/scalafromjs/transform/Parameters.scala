package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Transform._
import Symbols._
import Expressions._
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId, symId}

object Parameters {

  /*
  * AST types are incorrect - function parameters may be of a type other than Node.FunctionParameter
  * This allows us to hotfix this.
  * */
  def isNormalPar(par: Node.Node): Boolean = {
    //
    par match {
      case _: Node.FunctionParameter =>
        true
      case _ =>
        false
    }
  }

  case class FunctionBodyAndParams(body: Node.BlockStatement, params: Seq[Node.FunctionParameter]) {
    def replaceParam(name: Node.FunctionParameter, replace: Node.FunctionParameter) = {
      params.map { x =>
        if (x == name) replace
        else x
      }
    }

    def renameParam(oldParam: Node.FunctionParameter , newName: String) = {
      params.map { x =>
        if (x == oldParam) {
          val replacedId = Node.Identifier(newName)
          x match {
            case ap: Node.AssignmentPattern =>
              ap.left = replacedId.withTokens(ap.left)
              ap
            case nn: Node.Identifier =>
              replacedId.withTokens(nn)
          }
        }
        else x
      }
    }
  }

  private def notInlineBody(context: ScopeContext) = {
    context.parent().collect { case md: Node.MethodDefinition if methodName(md) == Classes.inlineBodyName => md }.isEmpty
  }

  /**
    * Scan all parameters in all functions, parameters are scaned from last to first (right to left)
    * @param process process the function - once this returns None, scan is aborted
    * */
  def processAllFunctions(n: Node.Node, process: (FunctionBodyAndParams, Node.FunctionParameter, ScopeContext) => Option[FunctionBodyAndParams]): Node.Node = {

    def processOneFunction[T <: Node.Node](f: Node.Node, context: ScopeContext): T = {

      def processArguments(f: FunctionBodyAndParams, args: Seq[Node.FunctionParameter]): FunctionBodyAndParams = args match {
        case Seq() =>
          f
        case head +: tail =>
          val processed = process(f, head, context)

          processed.fold(f)(processArguments(_, tail))
      }

      f match {
        case f: Node.FunctionExpression =>
          val func = processArguments(FunctionBodyAndParams(f.body, f.params), f.params.reverse)
          f.body = func.body
          f.params = func.params
          f.asInstanceOf[T]
        case f: Node.FunctionDeclaration =>
          val func = processArguments(FunctionBodyAndParams(f.body, f.params), f.params.reverse)
          f.body = func.body
          f.params = func.params
          f.asInstanceOf[T]
        case _ =>
          f.asInstanceOf[T]

      }
    }

    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case f: DefFun =>
          processOneFunction(f, ctx)
        case m: Node.MethodDefinition =>
          //println(s"introduceDefaultValues ${m.key.name}")
          ctx.withScope(m.value) {
            m.value = processOneFunction(m.value, ctx)
          }
          m
        case _ =>
          node
      }
    }
  }

  class CompareWithUndefined(parName: SymId)(implicit context: ScopeContext) {
    def unapply(arg: Node.Node) = arg match {
      case Binary(Node.Identifier(Id(`parName`)), op, Node.Identifier("undefined")) =>
        Some(op)
      case Binary(Node.Identifier(left), op, right) =>
        val sid = Id(left)
        if (sid == parName && right == Node.Identifier("undefined")) {
          Some(op)
        } else {
          None
        }
      case _ =>
        None
    }
  }

  def defaultValues(n: Node.Node): Node.Node = {

    // the only use of a parameter is in a `x_par || value` form
    def introduceDefaultValue(f: FunctionBodyAndParams, par: Node.FunctionParameter, context: ScopeContext): Option[FunctionBodyAndParams] = {
      implicit val ctx = context
      par match {
        case par: Node.Identifier =>
          if (!isNormalPar(par)) return None
          val parName = Id(parameterName(par)._1)
          //println(s"introduceDefaultValue $parName")

          // if there is only one reference, check if it is handling the default value
          object CompareParWithUndefined extends CompareWithUndefined(parName)

          object CheckParIsUndefined {
            def unapply(arg: Node.Node): Boolean = arg match {
              // par == undefined
              case CompareParWithUndefined("==" | "===") =>
                true
              // !par
              case Node.UnaryExpression("!", Node.Identifier(Id(`parName`))) =>
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
              case Node.Identifier(Id(`parName`)) =>
                true
              case _ =>
                false
            }
          }

          object IsParDefaultHandling {
            def unapply(arg: Node.Node) = arg match {
              case Binary(symRef@Node.Identifier(Id(`parName`)), "||", InitStatement(init)) =>
                Some(symRef, init)
              case Node.ConditionalExpression(CheckParNotUndefined(), symRef@Node.Identifier(Id(`parName`)), InitStatement(init)) =>
                Some(symRef, init)
              case Node.ConditionalExpression(CheckParIsUndefined(), InitStatement(init), symRef@Node.Identifier(Id(`parName`))) =>
                Some(symRef, init)
              case _ =>
                None
            }
          }

          object IsParDefaultHandlingAssignment {
            def unapply(arg: Node.Node) = arg match {
              case Node.ExpressionStatement(Node.AssignmentExpression("=", Node.Identifier(Id(`parName`)), IsParDefaultHandling(_, init))) =>
                Some(init)

              case Node.IfStatement(CheckParIsUndefined(), SingleExpression(Node.AssignmentExpression("=", Node.Identifier(Id(`parName`)), init)), IsNull()) =>
                Some(init)

              case _ => None
            }
          }

          var defValueCond = Option.empty[Node.Expression]
          var defValueAssign = Option.empty[Node.Expression]
          var otherUse = false
          var alreadyUsed = false
          f.body.walkWithScope(context) {(node, context) =>
            implicit val ctx = context
            node match {
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
              case Node.Identifier(Id(`parName`)) =>
                otherUse = true
                alreadyUsed = true
                defValueCond = None
                false
              case _ =>
                false
            }
          }

          defValueCond.toSeq ++ defValueAssign match { // use only one option - two are bad
            case Seq(init) =>
              val params = f.replaceParam(par, Node.AssignmentPattern(par, init))

              // remove the use
              val body = f.body.transformBefore { (node, descend, transform) =>
                node match {
                  case IsParDefaultHandlingAssignment(_) if defValueAssign.isDefined =>
                    Node.EmptyStatement()
                  case IsParDefaultHandling(symRef, _) if defValueCond.isDefined =>
                    symRef.clone()
                  case _ =>
                    descend(node.clone(), transform)
                }
              }
              Some(FunctionBodyAndParams(body, params))

            case _ =>
              None
          }
        case _ =>
          None
      }
    }


    processAllFunctions(n, introduceDefaultValue)

  }


  def modifications(n: Node.Node): Node.Node = {
    import VariableUtils._

    val refs = buildReferenceStacks(n)(new ScopeContext)

    def handleModification(f: FunctionBodyAndParams, par: Node.FunctionParameter, context: ScopeContext): Option[FunctionBodyAndParams] = {
      implicit val ctx = context

      for (parName <- nameFromPar(par)) yield {
        val parDef = Id(parName)
        //println(s"Checking $parName")

        // TODO: cloning destroys reference stacks - gather first

        // check if the parameter is ever modified
        val assignedInto = refs.isModified(parDef)
        if (assignedInto) {
          //println(s"Detected assignment into $parName")
          // we need to replace parameter x with x_par and intruduce var x = x_par

          val params = f.renameParam(par, parName + Symbols.parSuffix)

          val decl = VarDecl(parName, Some(Node.Identifier(parName + Symbols.parSuffix).withTokens(par)), "let", typeFromPar(par))(par)

          val body = decl +: f.body.body

          FunctionBodyAndParams(Node.BlockStatement(body).withTokens(f.body), params)

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

    def handleSimpleParameters(f: FunctionBodyAndParams, par: Node.FunctionParameter, context: ScopeContext): Option[FunctionBodyAndParams] = {
      if (notInlineBody(context)) {
        Some(f)
      } else {
        implicit val ctx = context
        val parName = parameterNameString(par)

        if (parName endsWith parSuffix) {
          // check for existence of variable without a suffix
          val shortName = parName dropRight parSuffix.length
          val conflict = f.body.body.exists {
            case VarDecl(`shortName`, _, _) =>
              true
            case _ =>
              false
          }
          if (!conflict) {
            val parDef = Id(parName)
            // we may rename the variable now
            //println(s"Renaming $s")
            val renamedBody = Variables.renameVariable(f.body, parDef, shortName)
            val params = f.renameParam(par, shortName)

            // rename hints as well
            types = types.renameHint(parDef, parDef.copy(name = shortName))

            Some(FunctionBodyAndParams(renamedBody, params))
          }
          else Some(f)
        } else Some(f)
      }
    }

    val ret = processAllFunctions(n.top, handleSimpleParameters).asInstanceOf[Node.Program]
    n.copy(top = ret, types = types)
  }


  def removeDeprecated(n: Node.Node): Node.Node = {
    def removeOneDeprecated(f: FunctionBodyAndParams, par: Node.FunctionParameter, context: ScopeContext): Option[FunctionBodyAndParams] = {
      implicit val ctx = context
      if (!isNormalPar(par)) return None
      val parName = Id(parameterName(par)._1)

      def containsDeprecation(body: Seq[Node.StatementListItem]) = {
        body.exists {
          case Node.ExpressionStatement(Node.CallExpression(Node.Identifier("console") Dot "warn", _)) =>
            true
          case _: Node.ThrowStatement =>
            true
          case _ =>
            false
        }
      }

      object IsParDeprecated {

        object CompareParWithUndefined extends CompareWithUndefined(parName)

        def unapply(arg: Node.Node) = arg match {
          case Node.IfStatement(CompareParWithUndefined("!=" | "!=="), Statements(body), IsNull()) if containsDeprecation(body) =>
            //println("IsParDeprecated")
            true
          case _ =>
            //println(s"no IsParDeprecated in ${nodeClassName(arg)}")
            false
        }
      }

      var isDeprecated = false
      var otherUse = false
      f.body.walkWithScope(context) {(node, scope) =>
        implicit val ctx = scope
        node match {
          case IsParDeprecated() =>
            //println(s"Detected deprecated par for $parName")
            isDeprecated = true
            true // use inside of the current pattern must not set otherUse
          case Node.Identifier(Id(`parName`)) =>
            otherUse = true
            true
          case _ =>
            otherUse
        }
      }

      if (isDeprecated && !otherUse) Some {
        FunctionBodyAndParams(
          f.body.transformAfter { (node, _) =>
            node match {
              case s@IsParDeprecated() =>
                Node.EmptyStatement()
              case _ =>
                node
            }
          },
          f.params diff Seq(par)
        )
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
    def handleConstructorVars(f: FunctionBodyAndParams, par: Node.FunctionParameter, context: ScopeContext): Option[FunctionBodyAndParams] = {
      if (notInlineBody(context)) {
        Some(f)
      } else {
        // inline all parameters, or constructor only?
        // check hints: is it a variable?
        val parSym = parameterName(par)._1
        val parDef = Id(parSym)(context)

        val isPurePar = n.types.getHint(Some(parDef)).contains(IsConstructorParameter)

        val parName = parSym.name
        if (logging) println(s"Checking par $parName")
        if (isPurePar) {
          if (logging) println(s"  is a pure parameter")
          object IsVarPar {

            def unapply(arg: Node.Node)(implicit ctx: ScopeContext) = arg match {
              case VarDecl(Id(symDef), Some(Node.Identifier(Id(`parDef`))), _) =>
                if (logging) println(s"IsVarPar $symDef ${parDef.name}")
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

          var isVarPar = Option.empty[SymId]
          var otherUse = false
          f.body.walkWithScope(context) { (node, context) =>
            implicit val ctx = context
            node match {
              case IsVarPar(varSym) =>
                if (logging) println(s"Detected variable par '${varSym.name}' for '$parName'")
                // only one var-par is allowed
                if (!otherUse && isVarPar.isEmpty) isVarPar = Some(varSym)
                else {
                  if (logging) println(s"  multiple variable par candidates for '$parName'")
                  isVarPar = None
                  otherUse = true
                }
                true // use inside of the current pattern must not set otherUse
              case Node.CallExpression(Node.ThisExpression() Dot "constructor", args) if args.exists(isParRef) =>
                // passed to the constructor - this is always allowed
                // should we check what the constructor does with the value?
                if (logging) println(s"Detected constructor call for '$parName'")
                true
              case Node.Identifier(`parName`) =>
                if (logging) println(s"Detected other use for '$parName'")
                isVarPar = None
                otherUse = true
                true
              case _ =>
                otherUse

            }
          }

          isVarPar.map { varSym =>
            if (logging) println(s"Rename '${varSym.name}'")

            val params = f.params.map {
              case nn@Node.Identifier(`parName`) =>
                Node.Identifier(varSym.name).withTokens(nn)
              case nn@Node.AssignmentPattern(Node.Identifier(`parName`), right) =>
                nn.left = Node.Identifier(varSym.name).withTokens(nn.left)
                nn
              case nn =>
                nn
            }
            // redirect also a symbol type
              //println(s"${parSym.name} -> ${varSym.name} Redirect type $tpe")
            for (tpe <- types.get(Some(parDef))) {
              types += Some(varSym) -> tpe
            }
            // remove the hint, the parameter is no longer pure
            types = types.copy(hints = types.hints - parDef)

            val body = f.body.transformBefore(context) { (node, descend, transformer) =>
              implicit val ctx = transformer.context
              node match {
                case IsVarPar(`varSym`) =>
                  Node.EmptyStatement()
                case Node.Identifier(Id(`parDef`)) =>
                  Node.Identifier(varSym.name).withTokens(node)
                case _ =>
                  descend(node, transformer)
                  node
              }
            }
            FunctionBodyAndParams(body, params)
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
