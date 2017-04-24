package com.github.opengrabeso

import Uglify.SymbolDef
import JsUtils._

import scala.language.implicitConversions

object SymbolTypes {


  sealed trait TypeDesc {
    def scalaConstruct: String = s"_"
    // should type be written explicitly when initializing a variable of this type?
    def typeOnInit: Boolean = true

    def knownItems: Int = 0
  }
  case class SimpleType(name: String) extends TypeDesc {
    override def toString = name

    override def knownItems = 1
  }
  case class ClassType(name: String) extends TypeDesc {
    override def toString = name

    override def knownItems = 1
  }
  case class ArrayType(elem: TypeDesc) extends TypeDesc {
    override def toString = s"Array[${elem.toString}]"

    override def scalaConstruct: String = s"Array.empty[$elem]"

    override def typeOnInit = false

    override def knownItems = super.knownItems + elem.knownItems

    def union(that: ArrayType)(implicit classOps: ClassOps) = ArrayType(typeUnion(elem, that.elem))
    def intersect(that: ArrayType)(implicit classOps: ClassOps) = ArrayType(typeIntersect(elem, that.elem))
  }

  case class MapType(elem: TypeDesc) extends TypeDesc {
    override def toString = s"Map[String, $elem]"

    override def scalaConstruct: String = s"Map.empty[String, $elem]"

    override def typeOnInit = false

    override def knownItems = super.knownItems + elem.knownItems

    def union(that: MapType)(implicit classOps: ClassOps) = MapType(typeUnion(elem, that.elem))
    def intersect(that: MapType)(implicit classOps: ClassOps) = MapType(typeIntersect(elem, that.elem))
  }


  case class FunctionType(ret: TypeDesc, args: IndexedSeq[TypeDesc]) extends TypeDesc {

    //println(s"FunctionType ${args.mkString("(",",",")")} => $ret")
    override def knownItems = super.knownItems + ret.knownItems + args.map(_.knownItems).sum

    override def toString = {
      def outputType(o: TypeDesc) = o.toString
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }

    def op(that: FunctionType, combine: (TypeDesc, TypeDesc) => TypeDesc)(implicit classOps: ClassOps) = {
      val ret = combine(this.ret, that.ret)
      val args = for ((a1, a2) <- this.args.zipAll(that.args, NoType, NoType)) yield {
        combine(a1, a2)
      }
      FunctionType(ret, args)
    }

    def union(that: FunctionType)(implicit classOps: ClassOps) = op(that, typeUnion)

    def intersect(that: FunctionType)(implicit classOps: ClassOps) = op(that, typeIntersect)
  }
  case object AnyType extends TypeDesc { // supertype of all
    override def toString = "Any"

  }
  case object NoType extends TypeDesc { // subtype of all
    override def toString = "Unit"
  }

  val any = SimpleType("Any")
  val number = SimpleType("Double")
  val boolean = SimpleType("Boolean")
  val string = SimpleType("String")

  def parseType(str: String): TypeDesc = {
    str match {
      case "number" => number
      case "boolean" => boolean
      case "string" => string
      case "any" => any
      case _ =>
        ClassType(str)

    }
  }

  // SymbolDef instances (including ids) are recreated on each figure_out_scope
  // we need a stable id. Original source location + name should be unique and stable
  case class SymbolMapId(name: String, sourcePos: Int)

  case class MemberId(cls: String, name: String)

  def memberId(maybeDesc: Option[String], name: String): Option[MemberId] = {
    maybeDesc.map(MemberId(_, name))
  }

  implicit def id(sym: SymbolDef): Option[SymbolMapId] = {
    val token = sym.orig.headOption.flatMap { _.start.nonNull }

    //println(s"id ${sym.name} ${sym.orig.map(UglifyExt.nodeClassName)} ${token.map(_.pos)}")
    token.map { t =>
      val pos = if (sym.global) 0 else t.pos
      SymbolMapId(sym.name, pos)
    }
  }

  def classFromType(tpe: Option[TypeInfo]): Option[String] = {
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
    //println(s"typeIntersect $tpe1, $tpe2")
    (tpe1, tpe2) match {
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
      case (c1: ClassType, _) => // while technically incorrect, we always prefer a class type against a non-class one
        c1
      case (_, c2: ClassType) =>
        c2
      case _ =>
        NoType // should be Nothing, but we rather keep any of the types
    }
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
      case (a1: MapType, a2: MapType) =>
        a1 union a2
      case _ =>
        AnyType
    }
  }

  private def typeFromOption(tpe: Option[TypeInfo]) = {
    tpe.getOrElse(TypeInfo(AnyType, NoType))
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
    val srcType = typeIntersect(t2.source, typeIntersect(t1.source, t2.sourceTypeFromTarget))
    //println(s"  intersect $t1 $t2 -> $srcType")
    Some(t1.copy(source = srcType))
  }

  def apply(): SymbolTypes = SymbolTypes.std
  def apply(syms: Seq[(SymbolDef, TypeDesc)]) = {
    val idMap = syms.map { case (k,v) => id(k) -> v }.toMap - None
    new SymbolTypes(StdLibraries(), idMap.map{ case (k, v) => k.get -> TypeInfo.target(v)})
  }

  case class ClassInfo(members: Map[String, Seq[String]] = Map.empty, parents: Map[String, String] = Map.empty) {

    lazy val children = parents.groupBy(_._2).mapValues(_.keys.toSet)

    //println(s"parents $parents")
    //println(s"children $children")
    //println(s"listChildren ${listChildren("X")}")

    def containsMember(cls: String, member: String): Boolean = {
      members.get(cls).exists(_.contains(member))
    }

    def classContains(cls: String, member: String): Option[String] = {
      val r = if (containsMember(cls, member)) Some(cls)
      else parents.get(cls).flatMap { c =>
        //println(s"  parent $c")
        classContains(c, member)
      }
      //println(s"Check $cls contains $member: $r")
      r
    }

    // list parents, the first in the list is the hierarchy root (no more parents), the last is the class itself
    def listParents(cls: String): Seq[String] = {
      def listParentsRecurse(cls: String, ret: Seq[String]): Seq[String] = {
        val p = parents.get(cls)
        p match {
          case Some(pp) => listParentsRecurse(pp, pp +: ret)
          case None => ret
        }
      }

      listParentsRecurse(cls, Seq(cls))
    }

    // list all children in no particular order
    def listChildren(cls: String): Set[String] = {
      def listChildrenRecurse(cls: String): Set[String] = {
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



    def mostDerived(c1: String, c2: String): Option[String] = {
      //println(s"  Parents of $c1: ${listParents(c1)}")
      //println(s"  Parents of $c2: ${listParents(c2)}")
      // check if one is parent of the other
      if (listParents(c1) contains c2) Some(c1)
      else if (listParents(c2) contains c1) Some(c2)
      else None
    }

    def commonBase(c1: String, c2: String): Option[String] = {
      val p1 = listParents(c1)
      val p2 = listParents(c2)
      (p1 zip p2).takeWhile(p => p._1 == p._2).lastOption.map(_._1)
    }

    def ++ (that: ClassInfo) = ClassInfo(this.members ++ that.members, this.parents ++ that.parents)

  }

  val libs = Map(
    "Math" -> Seq(
      "min", "max", "abs",
      "sin", "cos", "tan", "asin", "acos", "atan",
      "sqrt", "ceil", "floor", "round"
    )
  )
  val libNames = libs.keys.toSeq

  private val numberFunction = FunctionType(number, IndexedSeq(number))

  val stdLibraries: Map[SymbolMapId, TypeInfo] = libs.keys.map { k =>
    SymbolMapId(k, 0) -> TypeInfo.target(ClassType(k))// 0 is a special handling for global symbols
  }.toMap

  val stdLibraryMembers: Map[MemberId, TypeInfo] = (for {
    (cls, members) <- libs.toSeq
    member <- members
  } yield {
    MemberId(cls, member) -> TypeInfo.target(numberFunction) // TODO: type data driven
  })(collection.breakOut)


  // unique ID for std lib classes
  // once ClassType contains offset uniquely identifying a class, this class can be deleted
  case class StdLibraries(libs: Seq[String] = Seq.empty) {
    val index = libs.zipWithIndex.map {case (v, i) => v -> (-1 - i)}.toMap

    def symbolFromMember(cls: String, name: String): Option[SymbolMapId] = {
      val id = index.get(cls)
      id.map(SymbolMapId(name, _))
    }
  }

  val stdLibs = StdLibraries(libNames)

  val stdLibraryMemberSymbols: Map[SymbolMapId, TypeInfo] = stdLibraryMembers.toSeq.zipWithIndex.flatMap {case ((k,v), index) =>
    stdLibs.symbolFromMember(k.cls, k.name).map(_ -> v)
  }.toMap

  lazy val std: SymbolTypes = SymbolTypes(stdLibs, stdLibraries ++ stdLibraryMemberSymbols)

  lazy val stdClassInfo: ClassInfo = ClassInfo(libs, Map.empty)

}

import SymbolTypes._

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

}
case class TypeInfo(source: TypeDesc, target: TypeDesc) {
  def knownItems = source.knownItems max target.knownItems

  def nonEmpty = source != AnyType || target != NoType

  def known = source != AnyType && source!=NoType || target != NoType && target != AnyType

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

case class SymbolTypes(stdLibs: StdLibraries, types: Map[SymbolMapId, TypeInfo]) {

  def get(id: Option[SymbolMapId]): Option[TypeInfo] = id.flatMap(types.get)

  def symbolFromMember(memberId: MemberId): SymbolMapId = {
    ???
  }

  def getMember(clsId: Option[MemberId]): Option[TypeInfo] = get(clsId.map(symbolFromMember))
  def getMember(cls: Option[String], member: String): Option[TypeInfo] = getMember(cls.map(MemberId(_, member)))

  def getAsScala(id: Option[SymbolMapId]): String = {
    get(id).fold (any.toString) (t => t.declType.toString)
  }

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(stdLibs, types ++ that.types)

  def + (kv: (Option[SymbolMapId], TypeInfo)): SymbolTypes = {
    kv._1.fold(this) { id =>
      copy(types = types + (id -> kv._2))
    }
  }

  def addMember (kv: (Option[MemberId], TypeInfo)): SymbolTypes = {
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
