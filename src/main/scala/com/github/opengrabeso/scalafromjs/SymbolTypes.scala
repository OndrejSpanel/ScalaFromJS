package com.github.opengrabeso.scalafromjs

import scala.language.implicitConversions

import SymbolTypes._

object SymbolTypes {

  val watch = false

  def watchCondition(cond: => Boolean): Boolean = if (watch) cond else false

  def watched(name: String): Boolean = watchCondition {
    val watched = Set[String]("")
    name.startsWith("watch_") || watched.contains(name)
  }

  def watchedMember(cls: String, name: String): Boolean = watchCondition {
    val watched = Set[(String, String)](("Box3", "set"))
    val watchedAllClasses = Set[String]("")
    name.startsWith("watch_") || watched.contains(cls, name) || watchedAllClasses.contains(name)
  }

  def watchedSym(sym: SymbolMapId): Boolean = {
    watched(sym.name)
  }

  implicit class IsWatched(name: String) {
    def isWatched: Boolean = watched(name)
  }

  implicit class IsWatchedMember(id: MemberId) {
    def isWatched: Boolean = watchedMember(id.cls.name, id.name)
  }

  sealed trait TypeDesc {

    def scalaConstruct: String = "_"
    // should type be written explicitly when initializing a variable of this type?
    def typeOnInit: Boolean = true

    def knownItems: Int = 0

    def depthComplexity: Int = 0

    def acceptable: Boolean = depthComplexity <= 5

    def isSafeReplacementOf(that: TypeDesc): Boolean = {
      if (that ==  NoType || that == AnyType) {
        true
      } else {
        (this, that) match {
          case (ArrayType(thisElem), ArrayType(thatElem)) =>
            thisElem isSafeReplacementOf thatElem
          case (MapType(thisElem), MapType(thatElem)) =>
            thisElem isSafeReplacementOf thatElem
          case (FunctionType(thisRet, thisArgs), FunctionType(thatRet, thatArgs)) =>
            (thisRet isSafeReplacementOf thatRet) && (thisArgs zip thatArgs).forall(tt => tt._1 isSafeReplacementOf tt._2)
          case _ =>
            this == that
        }
      }
    }

    def toOut: String
  }
  case object ObjectOrMap extends TypeDesc {
    override def toOut = AnyType.toOut
    override def scalaConstruct: String = "new {}"
    override def typeOnInit = false
    override def knownItems = 0
  }
  case class SimpleType(name: String) extends TypeDesc {
    override def toString = name
    override def toOut = name

    override def knownItems = 1
  }
  case class ClassType(name: SymbolMapId) extends TypeDesc {
    override def toString = name.toString
    override def toOut = name.name

    override def knownItems = 1

    def isSafeReplacement(source: TypeDesc): Boolean = source == this
  }
  case class ArrayType(elem: TypeDesc) extends TypeDesc {
    override def toString = s"Array[${elem.toString}]"
    override def toOut = s"Array[${elem.toOut}]"

    override def scalaConstruct: String = s"Array.empty[${elem.toOut}]"

    override def typeOnInit = false

    override def knownItems = 1 + elem.knownItems

    override def depthComplexity = elem.depthComplexity + 1

    def union(that: ArrayType)(implicit classOps: ClassOps) = ArrayType(typeUnion(elem, that.elem))
    def intersect(that: ArrayType)(implicit classOps: ClassOps) = ArrayType(typeIntersect(elem, that.elem))
  }

  case class MapType(elem: TypeDesc) extends TypeDesc {
    override def toString = s"Map[String, ${elem.toString}]"
    override def toOut = s"Map[String, ${elem.toOut}]"

    override def scalaConstruct: String = s"Map.empty[String, ${elem.toOut}]"

    override def typeOnInit = false

    override def knownItems = 1 + elem.knownItems

    override def depthComplexity = elem.depthComplexity + 1

    def union(that: MapType)(implicit classOps: ClassOps) = MapType(typeUnion(elem, that.elem))
    def intersect(that: MapType)(implicit classOps: ClassOps) = MapType(typeIntersect(elem, that.elem))
  }


  case class FunctionType(ret: TypeDesc, args: IndexedSeq[TypeDesc]) extends TypeDesc {

    //println(s"FunctionType ${args.mkString("(",",",")")} => $ret")
    override def knownItems = 1 + ret.knownItems + args.map(_.knownItems).sum

    override def depthComplexity = ret.depthComplexity + args.headOption.map(_ => args.map(_.depthComplexity).max).getOrElse(0)

    override def toString = {
      def outputType(o: TypeDesc) = o.toString
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }

    override def toOut = {
      def outputType(o: TypeDesc) = o.toOut
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }

    def op(that: FunctionType, parsCombine: (TypeDesc, TypeDesc) => TypeDesc, retCombine: (TypeDesc, TypeDesc) => TypeDesc)(implicit classOps: ClassOps) = {
      val ret = retCombine(this.ret, that.ret)
      val args = for ((a1, a2) <- this.args.zipAll(that.args, NoType, NoType)) yield {
        parsCombine(a1, a2)
      }
      FunctionType(ret, args)
    }

    def union(that: FunctionType)(implicit classOps: ClassOps) = op(that, typeUnion, typeIntersect)

    def intersect(that: FunctionType)(implicit classOps: ClassOps) = op(that, typeIntersect, typeUnion)
  }
  case object AnyType extends TypeDesc { // supertype of all
    override def toString = "Any"
    override def toOut = "Any"

  }
  case object NoType extends TypeDesc { // subtype of all
    override def toString = "Unit"
    override def toOut = "Unit"
  }

  val any = SimpleType("Any")
  val number = SimpleType("Double")
  val boolean = SimpleType("Boolean")
  val string = SimpleType("String")

  def parseType(str: String): Option[TypeDesc] = {
    str match {
      case "number" => Some(number)
      case "boolean" => Some(boolean)
      case "string" => Some(string)
      case "any" => Some(any)
      case _ => None
    }
  }

  // SymbolDef instances (including ids) are recreated on each figure_out_scope
  // we need a stable id. Original source location + name should be unique and stable
  type SymbolMapId = esprima.symbols.SymId
  val SymbolMapId = esprima.symbols.SymId

  case class MemberId(cls: SymbolMapId, name: String)

  def memberId(maybeDesc: Option[SymbolMapId], name: String): Option[MemberId] = {
    maybeDesc.map(MemberId(_, name))
  }

  implicit def id(sym: SymbolMapId): Option[SymbolMapId] = {
    Some(sym)
  }

  def classFromType(tpe: Option[TypeInfo]): Option[SymbolMapId] = {
    tpe.map(_.declType) match {
      case Some(ClassType(name)) => Some(name)
      case _ => None
    }
  }

  trait ClassOps {
    def mostDerived(c1: ClassType, c2: ClassType): TypeDesc
    def commonBase(c1: ClassType, c2: ClassType): TypeDesc
  }

  object ClassOpsDummy extends ClassOps {
    def mostDerived(c1: ClassType, c2: ClassType): TypeDesc = c1
    def commonBase(c1: ClassType, c2: ClassType): TypeDesc = c2
  }

  // intersect: assignment source
  def typeIntersect(tpe1: TypeDesc, tpe2: TypeDesc)(implicit classOps: ClassOps): TypeDesc = {
    val r = (tpe1, tpe2) match {
      case _ if tpe1 == tpe2 =>
        tpe1
      case (t, AnyType) => t
      case (AnyType, t) => t
      case (t, NoType) => t // while technically incorrect, we always prefer some types against no type
      case (NoType, t) => t
      case (c1: ClassType, c2: ClassType) =>
        classOps.mostDerived(c1, c2)
      case (f1: FunctionType, f2: FunctionType) =>
        f1 intersect f2
      case (a1: ArrayType, a2: ArrayType) =>
        //println(s"a1 intersect a2 $a1 $a2")
        a1 intersect a2
      case (a1: MapType, a2: MapType) =>
        //println(s"a1 intersect a2 $a1 $a2")
        a1 intersect a2
      case (a: MapType, ObjectOrMap) => a
      case (ObjectOrMap, a: MapType) => a
      case (c1: ClassType, _) => // while technically incorrect, we always prefer a class type against a non-class one
        c1
      case (_, c2: ClassType) =>
        c2
      case _ =>
        NoType // should be Nothing, but we rather keep any of the types
    }
    //println(s"  typeIntersect $tpe1, $tpe2 = $r")
    r
  }

  // union: assignment target
  def typeUnion(tpe1: TypeDesc, tpe2: TypeDesc)(implicit classOps: ClassOps): TypeDesc = {
    (tpe1, tpe2) match {
      case _ if tpe1 == tpe2 =>
        tpe1
      case (_, AnyType) => AnyType
      case (AnyType, _) => AnyType
      case (t, NoType) => t
      case (NoType, t) => t
      case (c1: ClassType, c2: ClassType) =>
        classOps.commonBase(c1, c2)
      case (f1: FunctionType, f2: FunctionType) =>
        f1 union f2
      case (a1: ArrayType, a2: ArrayType) =>
        a1 union a2
      case (a1: MapType, a2: MapType) => a1 union a2
      case (ObjectOrMap, a: MapType) => a
      case (a: MapType, ObjectOrMap) => a
      case _ =>
        AnyType
    }
  }

  private def typeFromOption(tpe: Option[TypeInfo]) = {
    tpe.getOrElse(TypeInfo.unknown)
  }
  def typeUnionOption(tpe1: Option[TypeInfo], tpe2: Option[TypeInfo])(implicit classOps: ClassOps): Option[TypeInfo] = {
    val t1 = typeFromOption(tpe1)
    val t2 = typeFromOption(tpe2)
    val union = typeUnion(t1.target, t2.target)
    //println(s"  union $t1 $t2 -> $union")
    Some(t1.copy(target = union))
  }

  def typeIntersectOption(tpe1: Option[TypeInfo], tpe2: Option[TypeInfo])(implicit classOps: ClassOps): Option[TypeInfo] = {
    val t1 = typeFromOption(tpe1)
    val t2 = typeFromOption(tpe2)
    val srcType = typeIntersect(t1.declType, t2.declType)
    //println(s"  intersect $t1 $t2 -> ${t1.declType} x ${t2.declType} = $srcType")
    Some(t1.copy(source = srcType))
  }

  def apply(): SymbolTypes = SymbolTypes.std
  def apply(syms: Seq[(SymbolMapId, TypeDesc)]) = {
    val idMap = syms.map { case (k,v) => id(k) -> v }.toMap - None
    new SymbolTypes(StdLibraries(), idMap.map{ case (k, v) => k.get -> TypeInfo.target(v)})
  }

  case class ClassInfo(members: Map[SymbolMapId, Seq[String]] = Map.empty, parents: Map[SymbolMapId, SymbolMapId] = Map.empty) {

    lazy val children = parents.groupBy(_._2).mapValues(_.keys.toSet)

    //println(s"parents $parents")
    //println(s"children $children")
    //println(s"listChildren ${listChildren("X")}")

    def containsMember(cls: SymbolMapId, member: String): Boolean = {
      members.get(cls).exists(_.contains(member))
    }

    def classContains(cls: SymbolMapId, member: String): Option[SymbolMapId] = {
      val r = if (containsMember(cls, member)) Some(cls)
      else parents.get(cls).flatMap { c =>
        //println(s"  parent $c")
        classContains(c, member)
      }
      //println(s"Check $cls contains $member: $r")
      r
    }

    // list parents, the first in the list is the hierarchy root (no more parents), the last is the class itself
    def listParents(cls: SymbolMapId): Seq[SymbolMapId] = {
      def listParentsRecurse(cls: SymbolMapId, ret: Seq[SymbolMapId]): Seq[SymbolMapId] = {
        val p = parents.get(cls)
        p match {
          case Some(pp) => listParentsRecurse(pp, pp +: ret)
          case None => ret
        }
      }

      listParentsRecurse(cls, Seq(cls))
    }

    // list all children in no particular order
    def listChildren(cls: SymbolMapId): Set[SymbolMapId] = {
      def listChildrenRecurse(cls: SymbolMapId): Set[SymbolMapId] = {
        val p = children.get(cls)
        p match {
          case Some(pp) =>
            //println(s"  Some $pp")
            Set(cls) ++ pp.flatMap(listChildrenRecurse)
          case None =>
            //println(s"  None $cls")
            Set(cls)
        }
      }

      listChildrenRecurse(cls)
    }



    def mostDerived(c1: SymbolMapId, c2: SymbolMapId): Option[SymbolMapId] = {
      //println(s"  Parents of $c1: ${listParents(c1)}")
      //println(s"  Parents of $c2: ${listParents(c2)}")
      // check if one is parent of the other
      if (listParents(c1) contains c2) Some(c1)
      else if (listParents(c2) contains c1) Some(c2)
      else None
    }

    def commonBase(c1: SymbolMapId, c2: SymbolMapId): Option[SymbolMapId] = {
      val p1 = listParents(c1)
      val p2 = listParents(c2)
      (p1 zip p2).takeWhile(p => p._1 == p._2).lastOption.map(_._1)
    }

    def ++ (that: ClassInfo) = ClassInfo(this.members ++ that.members, this.parents ++ that.parents)

  }

  val libs = Map(
    // -1 is a special handling for global symbols
    SymbolMapId("Math", (-1, -1)) -> Seq(
      "min", "max", "abs",
      "sin", "cos", "tan", "asin", "acos", "atan",
      "sqrt", "ceil", "floor",
      "round"
    )
  )
  val libNames = libs.keys.toSeq

  private val numberFunction = FunctionType(number, IndexedSeq(number))

  val stdLibraries: Map[SymbolMapId, TypeInfo] = libs.keys.map { k =>
    k -> TypeInfo.target(ClassType(k))
  }.toMap

  val stdLibraryMembers: Map[MemberId, TypeInfo] = (for {
    (cls, members) <- libs.toSeq
    member <- members
  } yield {
    MemberId(cls, member) -> TypeInfo.target(numberFunction) // TODO: type data driven
  })(collection.breakOut)


  // unique ID for std lib classes
  // once ClassType contains offset uniquely identifying a class, this class can be deleted
  case class StdLibraries(libs: Seq[SymbolMapId] = Seq.empty) {
    val base = -10000
    val index = libs.zipWithIndex.map {case (v, i) => v -> (base - i)}.toMap

    def symbolFromClass(cls: SymbolMapId): SymbolMapId = {
      val id = index.get(cls)
      id.fold(cls)(c => cls.copy(sourcePos = c -> c))
    }

    def symbolFromMember(cls: SymbolMapId, name: String): Option[SymbolMapId] = {
      val id = index.get(cls)
      id.map(i => SymbolMapId(name, i -> i))
    }
  }

  val stdLibs = StdLibraries(libNames)

  val stdLibraryMemberSymbols: Map[SymbolMapId, TypeInfo] = stdLibraryMembers.toSeq.zipWithIndex.flatMap {case ((k,v), index) =>
    stdLibs.symbolFromMember(k.cls, k.name).map(_ -> v)
  }.toMap

  lazy val std: SymbolTypes = SymbolTypes(stdLibs, stdLibraries ++ stdLibraryMemberSymbols)

  lazy val stdClassInfo: ClassInfo = ClassInfo(libs, Map.empty)

}


object TypeInfo {
  def source(tpe: TypeDesc): TypeInfo = {
    //println(s"source $tpe")
    TypeInfo(tpe, NoType)
  }
  def target(tpe: TypeDesc): TypeInfo = {
    //println(s"target $tpe")
    TypeInfo(AnyType, tpe)
  }
  def both(tpe: TypeDesc): TypeInfo = {
    //println(s"both $tpe")
    TypeInfo(tpe, tpe)
  }
  def unknown: TypeInfo = TypeInfo(AnyType, NoType)

}
case class TypeInfo(source: TypeDesc, target: TypeDesc) {
  def equivalent(tp: TypeInfo): Boolean = source == tp.source && target == tp.target

  def knownItems = source.knownItems max target.knownItems

  def nonEmpty = source != AnyType || target != NoType
  def acceptable = source.acceptable && target.acceptable

  def known = source != AnyType && source!=NoType || target != NoType && target != AnyType

  def isSafeReplacementOf(that: TypeInfo): Boolean = {
    source.isSafeReplacementOf(that.source) && target.isSafeReplacementOf(that.target)
  }

  //assert(source == AnyType || target == NoType)
  // source should be more specific than a target (target is a supertype, source a subtype)
  // ... target is an upper bound, source a lower bound
  //assert(typeIntersect(source, target) == source)
  //assert(typeUnion(source, target) == target)

  def declType: TypeDesc = (source, target) match {
    case (_, NoType) => source
    case (AnyType, _) => target
    case _ => typeUnion(source, target)(ClassOpsDummy)
  }

  def sourceTypeFromTarget: TypeDesc = target match {
    case NoType => AnyType
    case AnyType => NoType
    case _ => target
  }

  def sourceFromTarget: TypeInfo = TypeInfo(sourceTypeFromTarget, NoType)

  def map(f: TypeDesc => TypeDesc): TypeInfo = TypeInfo(f(source), f(target))
}

sealed trait Hint

case object IsConstructorParameter extends Hint

/**
  Once locked, perform only "desperate" inference: i.e. from Unit to some type, never change a variable type
*/

case class SymbolTypes(stdLibs: StdLibraries, types: Map[SymbolMapId, TypeInfo], hints: Map[SymbolMapId, Hint] = Map.empty, locked: Boolean = false) {

  def get(id: Option[SymbolMapId]): Option[TypeInfo] = id.flatMap(types.get)
  def getHint(id: Option[SymbolMapId]): Option[Hint] = id.flatMap(hints.get)

  def renameHint(oldId: SymbolMapId, newId: SymbolMapId): SymbolTypes = {
    val mod = for (hint <- hints.get(oldId)) yield {
      var newHints = hints - oldId + (newId -> hint)
      copy(hints = newHints)
    }
    mod.getOrElse(this)
  }

  // TODO: move stdLibs and symbolFromMember out of SymbolTypes
  def symbolFromMember(memberId: MemberId)(implicit classPos: SymbolMapId => (Int, Int)): SymbolMapId = {
    // first check stdLibraries, if not found, try normal lookup
    stdLibs.symbolFromMember(memberId.cls, memberId.name).getOrElse {
      val clsPos = classPos(memberId.cls)
      SymbolMapId(memberId.name, clsPos)
    }
  }

  def getMember(clsId: Option[MemberId])(implicit classId: SymbolMapId => (Int, Int)): Option[TypeInfo] = get(clsId.map(symbolFromMember))

  def getAsScala(id: Option[SymbolMapId]): String = {
    get(id).fold (any.toOut) (t => t.declType.toOut)
  }

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(stdLibs, types ++ that.types, hints ++ that.hints, locked || that.locked)

  def + (kv: (Option[SymbolMapId], TypeInfo)): SymbolTypes = {
    kv._1.fold(this) { id =>
      /*
      if (id.name.startsWith("watchJS_")) {
        if (types.get(id).exists(_.equivalent(kv._2))) {
          println(s"++ Watched $id type == ${kv._2}")
        } else {
          println(s"++ Watched $id type ${kv._2}")
        }
      }
      */
      copy(types = types + (id -> kv._2))
    }
  }

  def addHint(kv: (Option[SymbolMapId], Hint)): SymbolTypes = {
    kv._1.fold(this) { id =>
      copy(hints = hints + (id -> kv._2))
    }

  }


  def addMember (kv: (Option[MemberId], TypeInfo))(implicit classId: SymbolMapId => (Int, Int)): SymbolTypes = {
    this + (kv._1.map(symbolFromMember) -> kv._2)
  }


  def knownItems: Int = {
    def sumTypeInfo(types: Iterable[TypeInfo]) = types.foldLeft(0)((s, i) => s + i.knownItems)
    sumTypeInfo(types.values)
  }

  override def toString = {
    types.mkString("{","\n", "}")
  }

}
