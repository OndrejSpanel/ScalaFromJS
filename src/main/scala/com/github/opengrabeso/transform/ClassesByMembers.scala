package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Transform._
import SymbolTypes._

import scala.language.implicitConversions

object ClassesByMembers {

  case class ClassDefInfo(members: Set[String], propMembers: Set[String], funMembers: Map[String, Int], parentCount: Int) {
    def + (that: ClassDefInfo): ClassDefInfo = ClassDefInfo(
      members ++ that.members,
      propMembers ++ that.propMembers,
      funMembers ++ that.funMembers,
      (parentCount max that.parentCount) + 1
    )
  }

  case class MemberList(classes: Map[SymbolMapId, AST_DefClass]) {

    case class ClassUseInfo(members: Set[String] = Set.empty, funMembers: Map[String, Int] = Map.empty) {
      def addMember(member: String): ClassUseInfo = copy(members = members + member)
      def addFunMember(member: String, pars: Int): ClassUseInfo = copy(funMembers = funMembers + (member -> pars))
    }

    var list = Map.empty[SymbolMapId, ClassUseInfo]

    val defList = {
      var members = Map.empty[SymbolMapId, ClassDefInfo]

      //println(s"classes ${classes.keys}")
      for {
        (clsName, cls) <- classes
      } {
        val parentName = superClass(cls)

        //println(s"Class $clsName parent $parentName")

        val propertiesSeq = cls.properties.toSeq
        val propertiesNonStatic = propertiesSeq.filterNot(propertyIsStatic)

        val funMembers = propertiesSeq.collect { case c: AST_ConciseMethod => c.key.name -> c.value.argnames.length }
        val getters = propertiesNonStatic.collect {case AST_ObjectGetter(AST_SymbolRefName(name), _) => name}
        val setters = propertiesNonStatic.collect {case AST_ObjectSetter(AST_SymbolRefName(name), _) => name}
        val values = propertiesNonStatic.collect {case c: AST_ObjectKeyVal => c.key}

        val propMembers = getters.toSet ++ setters.toSet ++ values.toSet

        val varMembers = for {
          body <- findInlineBody(cls).toSeq
          AST_Definitions(AST_VarDef(AST_SymbolName(varName), _)) <- body.value.body
        } yield varName

        //println(s"Cls ${id(cls.name.get.thedef.get)}: Fun $funMembers mem $varMembers")
        //println(s"Add def $cls $v")
        // inherit parent members
        val parentMembers = parentName.fold(Option.empty[ClassDefInfo])(members.get(_))

        val clsDef = ClassDefInfo(varMembers.toSet, propMembers, funMembers.toMap, 0)
        val updateCls = parentMembers.fold(clsDef)(_ + clsDef)
        members += clsName -> updateCls
      }
      members
    }

    def addMember(sym: SymbolDef, member: String) = {
      for (sid <- id(sym)) {
        //println(s"Add mem $sid $member")
        val memUpdated = list.get(sid).fold(ClassUseInfo(members = Set(member)))(_.addMember(member))
        list += sid -> memUpdated
      }
    }

    def addFunMember(sym: SymbolDef, member: String, pars: Int) = {
      for (sid <- id(sym)) {
        //println(s"Add fun mem $sid $member")
        val memUpdated = list.get(sid).fold(ClassUseInfo(funMembers = Map(member -> pars)))(_.addFunMember(member, pars))
        list += sid -> memUpdated
      }
    }

    def matchNames(use: String, cls: String): Int = {
      if (use.head.toLower == cls.head.toLower) 1 // prefer a class with the same first lecheck first identifier letter
      else 0
    }

    def bestMatch(useName: String, useInfo: ClassUseInfo, desperate: Boolean)(classInfo: ClassInfo): Option[SymbolMapId] = {
      defList.headOption.flatMap { _ =>

        val interesting = watched(useName)

        val candidates = defList.map { case (cls, ms) =>
          val msVars = ms.members ++ ms.propMembers
          val msFuns = ms.funMembers

          // prefer the class having most common members
          val score = (msVars intersect useInfo.members).size + (msFuns.toSet intersect useInfo.funMembers.toSet).size
          cls -> (ms, score)
        }

        // println(s"   candidates $candidates")

        val bestScore = candidates.maxBy(_._2._2)._2._2

        if (interesting) {
          println(s"  By members $useName: bestScore $bestScore")
          println(s"    used $useInfo")
        }

        val bestCandidatesIncludingChildren = candidates.filter(_._2._2 == bestScore)

        def removeChildren(list: List[SymbolMapId], ret: Set[SymbolMapId]): Set[SymbolMapId] = {
          if (list.isEmpty) {
            ret
          } else {
            val headWithChildren = classInfo.listChildren(list.head)
            removeChildren(list diff headWithChildren.toSeq, ret -- headWithChildren + list.head)
          }
        }

        val noChildren = removeChildren(bestCandidatesIncludingChildren.keySet.toList, bestCandidatesIncludingChildren.keySet)

        val bestCandidates = bestCandidatesIncludingChildren -- (bestCandidatesIncludingChildren.keySet -- noChildren)

        //if (bestCandidates.keySet != bestCandidatesIncludingChildren.keySet) println(s"    bestCandidates ${bestCandidates.keys}, with children ${bestCandidatesIncludingChildren.keys}")


        val maxCandidates = if (desperate) 5 else 1

        // if there are too many candidates or no match at all, assume nothing
        if (bestCandidates.size > maxCandidates || bestScore == 0) {
          None
        } else {
          // multiple candidates - we need to choose based on some secondary criterion
          if (interesting) println(s"    bestCandidates ${bestCandidates.keys}, with children ${bestCandidatesIncludingChildren.keys}")

          val best = bestCandidates.map { case (cls, (ms, score)) =>

            val r = (
              score, // prefer the class having most common members
              -ms.members.size, // prefer a smaller class
              -ms.parentCount, // prefer a less derived class
              matchNames(useName, cls.name), // prefer a class with a matching name
              cls // keep ordering stable, otherwise each iteration may select a random class
              //, ms.members intersect useInfo.members, ms.funMembers intersect useInfo.funMembers // debugging
            )
            //println(s"  Score $ms -> members: ${useInfo.members}, funs: ${useInfo.funMembers}: $r")
            r
          }.max //By(b => (b._1, b._2, b._3, b._4))


          // if there are no common members, do not infer any type
          // if too many members are unmatched, do not infer any type
          if (best._1 > 0 && best._1 > (useInfo.members.size + useInfo.funMembers.size) / 2) {
            val dumpUncertain = false
            if (dumpUncertain) {
              // matching only a few members from a large class
              if (best._1 < useInfo.members.size + useInfo.funMembers.size) {
                println(s"Suspicious $useInfo: Best $best, uses ${useInfo.members.size}+${useInfo.funMembers.size}")
              }
              else if (best._1 <= 2 && -best._2 > best._1) {
                println(s"Uncertain $useInfo: Best $best, uses ${useInfo.members.size}+${useInfo.funMembers.size}")
              }
            }

            //println(s"$useInfo: Best $best")
            Some(best._5)
          }
          else None
        }
      }
    }


  }

  def apply(n: AST_Extended, desperate: Boolean): AST_Extended = {

    // try to identify any symbol not inferred completely
    val classInfo = listDefinedClassMembers(n.top)
    val classes = new ClassListHarmony(n)
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes)

    val byMembers = MemberList(classes.classes)

    Time("byMembers.addMember") {
      n.top.walkWithDescend { (node, descend, walker) =>
        //println(s"by members walk $node")
        descend(node, walker)

        node match {
          case AST_SymbolRefDef(sym) AST_Dot member =>
            //println(s"Symbol ${sym.name}")
            val tpe = ctx.types.get(sym)
            if (tpe.isEmpty) {
              //println(s"Symbol ${sym.name} parent ${walker.parent().nonNull.map(nodeClassName)}")
              walker.parent().nonNull match {
                case Some(c: AST_Call) if c.expression == node =>
                  byMembers.addFunMember(sym, member, c.args.length)
                case _ =>
                  //println(s"  ${sym.name}.$member")
                  byMembers.addMember(sym, member)
              }
            }
          case _ =>
        }
        true
      }
    }

    Time("byMembers.bestMatch") {
      //println(s"List ${byMembers.list}")
      //println(s"Defs ${byMembers.defList}")
      // for each find the best match
      for {
        (sid, members) <- byMembers.list
        cls <- byMembers.bestMatch(sid.name, members, desperate)(classInfo)
      } {
        //println(s"Add type $s id $cls")

        if (watched(sid.name)) {
          println(s"Watched $sid class type by members $cls")
        }

        allTypes.t += Some(sid) -> TypeInfo.target(ClassType(cls))
      }
      n.copy(types = allTypes.t)
    }

  }
}
