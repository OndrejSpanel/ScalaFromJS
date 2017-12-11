package com.github.opengrabeso
package transform
package classes

import net.gamatron.esprima._
import esprima._

import Classes._
import Transform._

object FillVarMembers {
  def apply(n: NodeExtended): NodeExtended = {
    object IsThis {
      def unapply(arg: Node.Node) = arg match {
        case _: Node.This => true
        case Node.Identifier("this", _, _) => true // not used in practice, Node.This seems to catch all
        case _ => false
      }
    }

    // TODO: detect access other than this (see Node.This in expressionType to check general this handling)
    val retTop = n.top.transformAfter { (node, _) =>
      node match {
        case cls: Node.DefClass =>
          val accessor = findInlineBody(cls)
          //println(s"Node.DefClass ${cls.name.get.name} ${cls.start.get.pos}")
          var newMembers = collection.immutable.ListMap.empty[String, Option[Node.Node]]
          // scan known prototype members (both function and var) first
          val existingInlineMembers = accessor.map { _.value.body.toSeq.collect {
            case Node.Definitions(Node.VarDef(Node.SymbolName(name), _)) =>
              name
          }}.getOrElse(Seq())
          val existingParameters = accessor.map(_.value.argnames.toSeq.map(_.name)).getOrElse(Seq())

          val existingProtoMembers = listPrototypeMemberNames(cls)
          var existingMembers = existingProtoMembers ++ existingInlineMembers ++ existingParameters
          //println(s"  existingMembers ${existingProtoMembers.mkString(",")} inline ${existingInlineMembers.mkString(",")}")
          //println(s"  existingParameters ${existingParameters.mkString(",")}")

          cls.walk {
            case Node.Assign(IsThis() Node.StaticMemberExpression mem, _, _) =>
              //println(s"Detect this.$mem = ...")
              if (!existingMembers.contains(mem)) {
                newMembers += mem -> None
                existingMembers = existingMembers :+ mem
              } else for (prop <- isReadOnlyProperty(cls, mem)) {
                //println(s"Convert property to value $mem")
                newMembers += mem -> Some(prop)
              }
              false
            case IsThis() Node.StaticMemberExpression mem =>
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
            new Node.Var {
              fillTokens(this, clsTokenDef)
              val varDef = init.fold(Node.VarDef.uninitialized(clsTokenDef)(memberName))(Node.VarDef.initialized(clsTokenDef) (memberName, _))
              //println(s"fillVarMembers $memberName $varDef ${cls.start.get.pos} init $init")
              definitions = js.Array(varDef)
            }
          }

          if (vars.nonEmpty) {
            val accessor = classInlineBody(cls, clsTokenDef)

            accessor.body ++= (vars: Iterable[Node.Statement]).toJSArray
            //println(s"fillVarMembers newMembers $newMembers (${accessor.body.length})")

            // remove overwritten members
            cls.properties = cls.properties.filterNot(p => newMembers.contains(propertyName(p)))
          }

          cls
        case _ =>
          node
      }
    }

    val ret = n.copy(top = retTop)

    val classInfo = listClassMembers(ret)
    //println(s"Members ${classInfo.members}")

    // remove members already present in a parent from a derived class
    val cleanup = ret.top.transformAfter { (node, _) =>
      node match {
        case cls: Node.DefClass =>
          for {
            Node.Identifier(base) <- cls.`extends`
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

    ret.copy(top = cleanup)
  }


}
