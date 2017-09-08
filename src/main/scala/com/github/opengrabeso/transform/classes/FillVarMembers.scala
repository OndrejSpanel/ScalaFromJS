package com.github.opengrabeso
package transform
package classes

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Transform._
import TransformClasses._
import scala.scalajs.js
import js.JSConverters._

object FillVarMembers {
  def apply(n: AST_Node): AST_Node = {
    object IsThis {
      def unapply(arg: AST_Node) = arg match {
        case _: AST_This => true
        case AST_SymbolRef("this", _, _) => true // not used in practice, AST_This seems to catch all
        case _ => false
      }
    }

    // TODO: detect access other than this (see AST_This in expressionType to check general this handling)
    val ret = n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          val accessor = findInlineBody(cls)
          //println(s"AST_DefClass ${cls.name.get.name} ${cls.start.get.pos}")
          var newMembers = collection.immutable.ListMap.empty[String, Option[AST_Node]]
          // scan known prototype members (both function and var) first
          val existingInlineMembers = accessor.map { _.value.body.toSeq.collect {
            case AST_Definitions(AST_VarDef(AST_SymbolName(name), _)) =>
              name
          }}.getOrElse(Seq())
          val existingParameters = accessor.map(_.value.argnames.toSeq.map(_.name)).getOrElse(Seq())

          var existingMembers = listPrototypeMemberNames(cls) ++ existingInlineMembers ++ existingParameters
          //println(s"existingMembers $existingMembers proto ${listPrototypeMemberNames(cls)} inline ${existingInlineMembers}")

          cls.walk {
            case AST_Assign(IsThis() AST_Dot mem, _, _) =>
              //println(s"Detect this.$mem = ...")
              if (!existingMembers.contains(mem)) {
                newMembers += mem -> None
                existingMembers = existingMembers :+ mem
              } else for (prop <- isReadOnlyProperty(cls, mem)) {
                //println(s"Convert property to value $mem")
                newMembers += mem -> Some(prop)
              }
              false
            case IsThis() AST_Dot mem =>
              //println(s"Detect this.$mem")
              if (!existingMembers.contains(mem)) {
                newMembers += mem -> None
                existingMembers = existingMembers :+ mem
              }
              false

            case _ =>
              false
          }

          val clsTokenDef = classTokenSource(cls)
          val vars = newMembers.map { case (memberName, init) =>
            new AST_Var {
              fillTokens(this, clsTokenDef)
              val varDef = init.fold(AST_VarDef.uninitialized(clsTokenDef)(memberName))(AST_VarDef.initialized(clsTokenDef) (memberName, _))
              //println(s"fillVarMembers $varDef ${cls.start.get.pos} init $init")
              definitions = js.Array(varDef)
            }
          }

          if (vars.nonEmpty) {
            val accessor = classInlineBody(cls, clsTokenDef)

            accessor.body ++= (vars: Iterable[AST_Statement]).toJSArray
            //println(s"fillVarMembers newMembers $newMembers (${accessor.body.length})")

            // remove overwritten members
            cls.properties = cls.properties.filterNot(p => newMembers.contains(propertyName(p)))
          }

          cls
        case _ =>
          node
      }
    }

    val classInfo = listClassMembers(ret)
    //println(s"Members ${classInfo.members}")

    // remove members already present in a parent from a derived class
    val cleanup = ret.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          for {
            AST_SymbolDef(base) <- cls.`extends`
            baseId <- SymbolTypes.id(base)
            inlineBody <- findInlineBody(cls)
          } {
            //println(s"Detected base $base")
            inlineBody.value.body = inlineBody.value.body.filter {
              case VarName(member) => classInfo.classContains(baseId, member).isEmpty
              case _ => true
            }
          }
          cls
        case _ =>
          node
      }
    }

    cleanup
  }


}
