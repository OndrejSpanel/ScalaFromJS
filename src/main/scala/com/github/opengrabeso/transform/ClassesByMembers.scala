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

  case class ClassDefInfo(members: Set[String], funMembers: Set[String], parentCount: Int)

  case class MemberList(classInfo: ClassInfo, types: SymbolTypes) {

    case class ClassUseInfo(members: Set[String], funMembers: Set[String]) {
      def addMember(member: String): ClassUseInfo = copy(members = members + member)
      def addFunMember(member: String): ClassUseInfo = copy(funMembers = funMembers + member)
    }

    var list = Map.empty[SymbolMapId, ClassUseInfo]

    val defList = {
      var members = Map.empty[String, ClassDefInfo]

      //println(s"classInfo.members ${classInfo.members}")
      for {
        (c, v) <- classInfo.members
        cls <- classInfo.listChildren(c)
      } {
        //println(s"Cls $cls")
        // partition members to function and normal
        val (funMembers, varMembers) = v.partition { member =>
          val memberType = types.getMember(Some(cls), member)
          //println(s"  member $member: $memberType")
          // when type not known, assume function (safer)
          memberType.forall(_.declType.isInstanceOf[FunctionType])

        }
        //println(s"Cls $cls: Fun $funMembers mem $varMembers")
        //println(s"Add def $cls $v")
        val updateCls = members.get(cls).fold {
          ClassDefInfo(varMembers.toSet, funMembers.toSet, 0)
        } { m =>
          ClassDefInfo(m.members ++ varMembers.toSet, m.funMembers ++ funMembers.toSet, m.parentCount + 1)
        }
        members += cls -> updateCls
      }
      members
    }

    def addMember(sym: SymbolDef, member: String) = {
      for (sid <- id(sym)) {
        //println(s"Add mem $sid $member")
        val memUpdated = list.get(sid).fold {
          ClassUseInfo(Set(member), Set())
        }(_.addMember(member))
        list += sid -> memUpdated
      }
    }

    def addFunMember(sym: SymbolDef, member: String) = {
      for (sid <- id(sym)) {
        //println(s"Add mem $sid $member")
        val memUpdated = list.get(sid).fold {
          ClassUseInfo(Set(), Set(member))
        }(_.addFunMember(member))
        list += sid -> memUpdated
      }
    }

    def matchNames(use: String, cls: String): Int = {
      if (use.head.toLower == cls.head.toLower) 1 // prefer a class with the same first lecheck first identifier letter
      else 0
    }

    def bestMatch(useName: String, useInfo: ClassUseInfo): Option[String] = {
      defList.headOption.flatMap { _ =>
        val best = defList.map { case (cls, ms) =>
          (
            (ms.members intersect useInfo.members).size + (ms.funMembers intersect useInfo.funMembers).size, // prefer the class having most common members
            -ms.members.size, // prefer a smaller class
            -ms.parentCount, // prefer a less derived class
            matchNames(useName, cls), // prefer a class with a matching name
            cls // keep ordering stable, otherwise each iteration may select a random class
            //, ms.members intersect useInfo.members, ms.funMembers intersect useInfo.funMembers // debugging
          )
          //println(s"  Score $ms -> $members: $r")
        }.max //By(b => (b._1, b._2, b._3, b._4))
        // if there are no common members, do not infer any type
        if (best._1 > 0) {
          //println(s"$useInfo: Best $best")
          Some(best._5)
        }
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

    val byMembers = MemberList(classInfo, allTypes.t)

    n.top.walkWithDescend { (node, descend, walker) =>
      //println(s"${nodeClassName(node)}")
      descend(node, walker)

      node match {
        case cls@AST_SymbolRefDef(sym) AST_Dot member =>
          val tpe = expressionType(cls)(ctx)
          if (tpe.isEmpty) {
            walker.parent().nonNull match {
              case Some(_: AST_Call) =>
                byMembers.addFunMember(sym, member)
              case _ =>
                //println(s"  $tpe.$member -- ${sym.name}")
                byMembers.addMember(sym, member)
            }
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
      cls <- byMembers.bestMatch(sid.name, members)
    } {
      //println(s"Add type $sid $cls")
      allTypes.t += Some(sid) -> TypeInfo.target(ClassType(cls))
    }
    n.copy(types = allTypes.t)

  }
}