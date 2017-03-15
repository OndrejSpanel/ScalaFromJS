package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._
import scalajs.js

object Parameters {

  def processAllFunctions(n: AST_Node, removeDeprecatedValue: (AST_Lambda, AST_SymbolFunarg) => Option[AST_Lambda]): AST_Node = {

    def processOneFunction(f: AST_Lambda): AST_Lambda = {

      def processArguments(f: AST_Lambda, args: Seq[AST_SymbolFunarg]): AST_Lambda = args match {
        case Seq() =>
          f
        case head +: tail =>
          removeDeprecatedValue(f, head).fold(f) {
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
        object SingleStatement {
          def unapply(arg: AST_Statement) = arg match {
            case AST_BlockStatement(Seq(AST_SimpleStatement(body))) => Some(body)
            case AST_SimpleStatement(body) => Some(body)
            case _ => None
          }
        }
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

  def removeDeprecated(n: AST_Node): AST_Node = {
    def removeOneDeprecated(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      val parName = par.name
      //println(s"removeOneDeprecated $parName")

      object Statements {
        def unapply(arg: AST_Node) = arg match {
          case AST_BlockStatement(body) => Some(body)
          case s@AST_SimpleStatement(body) => Some(Seq(s))
          case _ => None
        }
      }

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
}
