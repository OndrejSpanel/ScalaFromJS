package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Transform._
import SymbolTypes._

import scala.scalajs.js
import scala.language.implicitConversions

object ClassesByMembers {

  case class ClassDefInfo(members: Set[String], parentCount: Int)

  case class MemberList(classInfo: ClassInfo) {
    var list = Map.empty[SymbolMapId, Set[String]]

    val defList = {
      var members = Map.empty[String, ClassDefInfo]

      //println(s"classInfo.members ${classInfo.members}")
      for {
        (c, v) <- classInfo.members
        cls <- classInfo.listChildren(c)
      } {
        //println(s"Add def $cls $v")
        members.get(cls).fold {
          members += cls -> ClassDefInfo(v.toSet, 0)
        } { m =>
          members += cls -> ClassDefInfo(m.members ++ v, m.parentCount + 1)
        }
      }
      members
    }

    def addMember(sym: SymbolDef, member: String) = {
      for (sid <- id(sym)) {
        //println(s"Add mem $sid $member")
        list.get(sid).fold[Unit] {
          list += sid -> Set(member)
        } {
          members =>
            list += sid -> (members + member)
        }
      }
    }

    def bestMatch(members: Set[String]): Option[String] = {
      defList.headOption.flatMap { _ =>
        val best = defList.map { case (cls, ms) =>
          (
            (ms.members intersect members).size, // prefer the class having most common members
            -ms.members.size, // prefer a smaller class
            -ms.parentCount, // prefer a less derived class
            cls // keep ordering stable, otherwise each iteration may select a random class
          )
          //println(s"  Score $ms -> $members: $r")
        }.max
        //println(s"Best $m for $members")
        // if there are no common members, do not infer any type
        if (best._1 > 0) Some(best._4)
        else None
      }
    }


  }

  def apply(n: AST_Extended): AST_Extended = {

    // try to identify any symbol not inferred completely
    val classInfo = listDefinedClassMembers(n.top)
    val classes = classListHarmony(n)
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes)

    val byMembers = MemberList(classInfo)

    n.top.walkWithDescend { (node, descend, walker) =>
      //println(s"${nodeClassName(node)}")
      descend(node, walker)

      node match {
        // TODO: accept member access as well, like in SymbolAccess
        case cls@AST_SymbolRefDef(sym) AST_Dot member =>
          val tpe = expressionType(cls)(ctx)
          if (tpe.isEmpty) {
            //println(s"  $tpe.$member -- ${sym.name}")
            byMembers.addMember(sym, member)
          }
        case _ =>
      }
      true
    }

    //println(s"List ${byMembers.list}")
    //println(s"Defs ${byMembers.defList}")
    // for each find the best match
    for {
      (sid, members) <- byMembers.list
      cls <- byMembers.bestMatch(members)
    } {
      //println(s"Add type $sid $cls")
      allTypes.t += Some(sid) -> TypeInfo.target(ClassType(cls))
    }
    n.copy(types = allTypes.t)

  }
}
