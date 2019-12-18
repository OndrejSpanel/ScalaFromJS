package com.github.opengrabeso.scalafromjs
package transform
package classes

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import Transform._
import com.github.opengrabeso.scalafromjs.esprima.symbols.Id

object FillVarMembers {
  def apply(n: NodeExtended): NodeExtended = {
    object IsThis {
      def unapply(arg: Node.Node) = arg match {
        case _: Node.ThisExpression => true
        case Node.Identifier("this") => true // not used in practice, Node.This seems to catch all
        case _ => false
      }
    }

    // TODO: detect access other than this (see Node.This in expressionType to check general this handling)
    val retTop = n.top.transformAfter { (node, _) =>
      node match {
        case cls: Node.ClassDeclaration =>
          val accessor = findInlineBody(cls).flatMap(getMethodMethod)
          //println(s"Node.ClassDeclaration ${cls.name.get.name} ${cls.start.get.pos}")
          var newMembers = collection.immutable.ListMap.empty[String, (Node.Node, Option[Node.Expression])]
          // scan known prototype members (both function and var) first
          val existingInlineMembers = accessor.map { _.body.body.collect {
            case vd@VarDecl(name, _, _) =>
              name
          }}.getOrElse(Seq())
          val existingParameters = accessor.map(_.params.map(parameterNameString)).getOrElse(Seq())

          val existingProtoMembers = listPrototypeMemberNames(cls)
          var existingMembers = existingProtoMembers ++ existingInlineMembers ++ existingParameters
          //println(s"  existingMembers ${existingProtoMembers.mkString(",")} inline ${existingInlineMembers.mkString(",")}")
          //println(s"  existingParameters ${existingParameters.mkString(",")}")

          cls.walk {
            case n@Assign(IsThis() Dot mem, _, _) =>
              //println(s"Detect this.$mem = ...")
              if (!existingMembers.contains(mem)) {
                newMembers += mem -> (n, None)
                existingMembers = existingMembers :+ mem
              } else for (prop <- isReadOnlyProperty(cls, mem)) {
                //println(s"Convert property to value $mem")
                newMembers += mem -> (n, Some(prop))
              }
              false
            case n@(IsThis() Dot mem) =>
              //println(s"Detect this.$mem")
              if (!existingMembers.contains(mem)) {
                newMembers += mem -> (n, None)
                existingMembers = existingMembers :+ mem
              }
              false

            case _ =>
              false
          }

          val clsTokenDef = classTokenSource(cls)
          val vars = newMembers.map { case (memberName, init) =>
            VarDecl(memberName, init._2, "var")(init._1)
          }

          if (vars.nonEmpty) {
            for (accessor <- getMethodBody(classInlineBody(cls, clsTokenDef))) {

              accessor.body ++= vars
              //println(s"fillVarMembers newMembers $newMembers (${accessor.body.length})")

              // remove overwritten members
              cls.body.body = cls.body.body.filterNot(p => newMembers.contains(methodName(p)))
            }
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
    val cleanup = ret.top.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cls: Node.ClassDeclaration =>
          for {
            Node.Identifier(Id(baseId)) <- Option(cls.superClass)
            inlineBody <- findInlineBody(cls)
            body <- getMethodBody(inlineBody)
          } {
            //println(s"Detected base $base")
            body.body = body.body.filter {
              case VarDecl(member, _, _) => classInfo.classContains(baseId, member).isEmpty
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
