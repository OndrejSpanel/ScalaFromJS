package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import Transform._
import SymbolTypes._
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, SymId}
import scala.collection.Seq

import scala.language.implicitConversions

object ClassesByMembers {

  /**
    * Match what members are used by a symbol against a list of all classes
    * defList is a list of all known classes
    * byMembers is a list of member usages for symbols
    * */
  case class ClassDefInfo(members: Set[String], propMembers: Set[String], funMembers: Map[String, Int], parentCount: Int) {
    def + (that: ClassDefInfo): ClassDefInfo = ClassDefInfo(
      members ++ that.members,
      propMembers ++ that.propMembers,
      funMembers ++ that.funMembers,
      (parentCount max that.parentCount) + 1
    )
  }

  case class MemberList(classes: Map[SymbolMapId, (Option[SymId], Node.ClassDeclaration)]) {

    case class ClassUseInfo(members: Set[String] = Set.empty, funMembers: Map[String, Int] = Map.empty) {
      def addMember(member: String): ClassUseInfo = {
        // detecting by length makes more harm then good, as this is used for arrays
        val ignored = Set("length")
        if (ignored contains member) this
        else copy(members = members + member)
      }
      def addFunMember(member: String, pars: Int): ClassUseInfo = copy(funMembers = funMembers + (member -> pars))
    }

    var list = Map.empty[SymbolMapId, ClassUseInfo]

    val defList = {
      var members = Map.empty[SymbolMapId, ClassDefInfo]

      //println(s"classes ${classes.keys}")
      for ((clsName, (parentName, cls)) <- classes) {

        //println(s"Class $clsName parent $parentName")

        val propertiesSeq = Option(cls.body).map(_.body).getOrElse(Nil)
        val propertiesNonStatic = propertiesSeq.filterNot(propertyIsStatic)


        def listKind(seq: Seq[Node.ClassBodyElement], kind: String) = seq.collect {
          case c: Node.MethodDefinition if hasName(c) && c.kind == kind =>
            methodName(c) -> getMethodMethod(c).fold(0)(_.params.length)
        }

        val funMembers = listKind(propertiesSeq, "init") ++ listKind(propertiesSeq, "method")

        val getters = listKind(propertiesNonStatic, "get").map(_._1)
        val setters = listKind(propertiesNonStatic, "set").map(_._1)
        val values = listKind(propertiesNonStatic, "value").map(_._1) // TODO: how is value represented in ES6?

        val propMembers = getters.toSet ++ setters.toSet ++ values.toSet

        val varMembers = for {
          body <- findInlineBody(cls).toSeq
          method <- getMethodBody(body).toSeq
          VarDecl(varName, _, _) <- method.body
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

    def addMember(sym: SymId, member: String) = {
      for (sid <- id(sym)) {
        //println(s"Add mem $sid $member")
        val memUpdated = list.getOrElse(sid, ClassUseInfo()).addMember(member)
        list += sid -> memUpdated
      }
    }

    def addFunMember(sym: SymId, member: String, pars: Int) = {
      for (sid <- id(sym)) {
        //println(s"Add fun mem $sid $member")
        val memUpdated = list.getOrElse(sid, ClassUseInfo()).addFunMember(member, pars)
        list += sid -> memUpdated
      }
    }

    def bestMatch(useName: String, useInfo: ClassUseInfo, desperate: Boolean)(classInfo: ClassInfo): Option[SymbolMapId] = {
      defList.headOption.flatMap { _ => // avoid processing when there are no definitions (would crash)

        val interesting = watched(useName)

        def nameMatch(varName: String, className: String): Int = {
          val v = varName.toLowerCase
          val c = className.toLowerCase
          if (v == c) 4
          else if (c.startsWith(v)) 3
          else if (v.length >= 2 && c.contains(v)) 2
          else if (c.head == v.head) 1
          else 0
        }

        // compute score for each candidate
        val candidates = defList.map { case (cls, ms) =>
          val msVars = ms.members ++ ms.propMembers
          val msFuns = ms.funMembers

          // prefer the class having most common members
          val score = (msVars intersect useInfo.members).size + (msFuns.toSet intersect useInfo.funMembers.toSet).size
          cls -> (ms, score)
        }

        // println(s"   candidates $candidates")

        val bestScore = candidates.maxBy(_._2._2)._2._2

        // ignore match in one member unless desperate
        if (bestScore <= 0 || !desperate && bestScore <=1 ) return None

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

        val bestCandidates = bestCandidatesIncludingChildren.view.filterKeys(noChildren.contains).toMap

        //if (bestCandidates.keySet != bestCandidatesIncludingChildren.keySet) println(s"    bestCandidates ${bestCandidates.keys}, with children ${bestCandidatesIncludingChildren.keys}")


        // if there are too many candidates or no match at all, assume nothing
        if (bestCandidates.size > 1 && !desperate) {
          if (interesting) println(s"    too many bestCandidates ${bestCandidates.keys.size}")
          None
        } else {
          // multiple candidates - we need to choose based on some secondary criterion
          if (interesting) println(s"    bestCandidates ${bestCandidates.keys}, with children ${bestCandidatesIncludingChildren.keys}")

          if (bestCandidates.size == 1) {
            Some(bestCandidates.head._1)
          } else {
            val matchByName = bestCandidates.keys.map { cls =>
              cls -> nameMatch(useName, cls.name)
            }
            val bestMatchByNameScore = matchByName.map(_._2).max
            val bestMatchByName = matchByName.filter(_._2 == bestMatchByNameScore)

            // if match by name produces a single sensible result, use it
            if (bestMatchByName.size == 1 && bestMatchByNameScore > 1) {
              val best = bestMatchByName.head

              if (interesting) println(s"    choose by name ${best._1.name} for $useName")
              Some(best._1)
            }
            else None
          }
        }
      }
    }


  }

  def apply(n: NodeExtended, desperate: Boolean): NodeExtended = {

    // try to identify any symbol not inferred completely
    val classInfo = listDefinedClassMembers(n)
    val classes = ClassListHarmony.fromAST(n.top)
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes)

    val byMembers = MemberList(classes.classes)

    Time("byMembers.addMember") {
      n.top.walkWithScopeAfter { (node, walker) =>
        //println(s"by members walk $node")
        implicit val scopeCtx = walker

        node match {
          case Node.Identifier(Id(sym)) Dot member =>
            //println(s"Symbol ${sym.name}")
            val tpe = ctx.types.get(sym)
            if (tpe.isEmpty) {
              //println(s"Symbol ${sym.name} parent ${walker.parent().map(nodeClassName)}")
              walker.parent() match {
                case Some(Node.CallExpression(`node`, args))  =>
                  byMembers.addFunMember(sym, member, args.length)
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
