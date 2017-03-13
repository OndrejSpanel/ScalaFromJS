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

  def defaultValues(n: AST_Node): AST_Node = {

    // the only use of a parameter is in a `x_par || value` form
    def introduceDefaultValue(f: AST_Lambda, par: AST_SymbolFunarg): Option[AST_Lambda] = {
      val parName = par.name
      //println(s"introduceDefaultValue $parName")
      val references = par.thedef.nonNull.toSeq.flatMap(_.references)

      val defByOr = if (references.size == 1) {
        // if there is only one reference, check if it is handling the default value
        object IsParDefaultHandling {
          def unapply(arg: AST_Node) = arg match {
            case AST_Binary(symRef@AST_SymbolRefName(`parName`), "||", init: AST_Constant) => Some(symRef, init)
            case _ =>
              None
          }
        }

        var defValue = Option.empty[AST_Node]
        f.walk {
          case IsParDefaultHandling(_, init) =>
            //println(s"Detected def value for $parName")
            defValue = Some(init)
            true // use inside of the def. value pattern must not set otherUse
          case _ =>
            defValue.isDefined
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

      } else None

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
            AST_Binary(symRef@AST_SymbolRefName(`parName`), "==" | "===", AST_SymbolRefName("undefined")),
            SingleStatement(AST_Assign(AST_SymbolRefName(`parName`), "=", init: AST_Constant)),
            None
            ) =>
              //println("IsParDefaultHandling via if")
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
      None
    }

    processAllFunctions(n, removeOneDeprecated)

  }
}
