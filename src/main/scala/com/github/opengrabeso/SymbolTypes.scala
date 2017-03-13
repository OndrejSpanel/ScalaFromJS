package com.github.opengrabeso

import Uglify.SymbolDef
import JsUtils._

import scala.language.implicitConversions

object SymbolTypes {


  sealed trait TypeDesc
  case class SimpleType(name: String) extends TypeDesc {
    override def toString = name
  }
  case class ClassType(name: String) extends TypeDesc {
    override def toString = name
  }
  case class FunctionType(ret: TypeDesc, args: IndexedSeq[TypeDesc]) extends TypeDesc {
    override def toString = {
      def outputType(o: TypeDesc) = o.toString
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }
  }
  case object AnyType extends TypeDesc { // supertype of all
    override def toString = "Any"
  }
  case object NoType extends TypeDesc { // subtype of all
    override def toString = "Nothing"
  }

  private val numberStr = "number"
  private val booleanStr = "boolean"
  private val stringStr = "string"
  private val anyStr = "Any"

  val any = SimpleType(anyStr)
  val number = SimpleType(numberStr)
  val boolean = SimpleType(booleanStr)
  val string = SimpleType(stringStr)

  /* it would be tempting to use something like _! to avoid possible clashes with other identifiers
  That would hover require to always add a traling space or to use `around the symbol` to prevent any following operator
  be considered a part of the identifier, as in name_!: Any
  */
  val parSuffix = "_par"


  def parseType(str: String): TypeDesc = {
    str match {
      case `numberStr` | `booleanStr` | `stringStr` | `anyStr` =>
        SimpleType(str)
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

  def mapSimpleTypeToScala(tpe: TypeDesc): String = {
    tpe match {
      case `string` => "String"
      case `number` => "Double"
      case `boolean` => "Boolean"
      case _ => tpe.toString
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

  // intersect: assignment source
  def typeIntersect(tpe1: TypeDesc, tpe2: TypeDesc)(implicit classOps: ClassOps): TypeDesc = {
    //println(s"typeIntersect $tpe1, $tpe2")
    (tpe1, tpe2) match {
      case _ if tpe1 == tpe2 =>
        tpe1
      case (t, AnyType) => t
      case (AnyType, t) => t
      case (_, NoType) => NoType
      case (NoType, _) => NoType
      case (c1: ClassType, c2: ClassType) =>
        classOps.mostDerived(c1, c2)
      case (c1: ClassType, _) =>
        c1
      case (_, c2: ClassType) =>
        c2
      case _ =>
        tpe1 // should be Nothing, but we rather keep any of the types
    }
  }

  def typeUnionFunction(f1: FunctionType, f2: FunctionType)(implicit classOps: ClassOps): TypeDesc = {
    val ret = typeIntersect(f1.ret, f2.ret)
    val args = for ((a1, a2) <- f1.args.zipAll(f2.args, NoType, NoType)) yield {
      typeUnion(a1, a2)
    }
    FunctionType(ret, args)
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
        typeUnionFunction(f1, f2)
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
    Some(t1.copy(target = typeUnion(t1.target, t2.target)))
  }

  def typeIntersectOption(tpe1: Option[TypeInfo], tpe2: Option[TypeInfo])(implicit classOps: ClassOps): Option[TypeInfo] = {
    val t1 = typeFromOption(tpe1)
    val t2 = typeFromOption(tpe2)
    //println(s"  intersect $t1 $t2")
    val srcType = typeIntersect(t2.source, typeIntersect(t1.source, t2.sourceTypeFromTarget))
    Some(t1.copy(source = srcType))
  }

  def apply(): SymbolTypes = SymbolTypes.std
  def apply(syms: Seq[(SymbolDef, TypeDesc)]) = {
    val idMap = syms.map { case (k,v) => id(k) -> v }.toMap - None
    new SymbolTypes(idMap.map{ case (k, v) => k.get -> TypeInfo.target(v)}, Map.empty)
  }

  case class ClassInfo(members: Set[MemberId] = Set.empty, parents: Map[String, String] = Map.empty) {

    def classContains(cls: String, member: String): Option[String] = {
      val r = if (members contains MemberId(cls, member)) Some(cls)
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
  }

  val numberMath = Seq(
    "min", "max", "abs",
    "sin", "cos", "tan", "asin", "acos", "atan",
    "sqrt", "ceil", "floor", "round"
  )

  val stdLibraries = Seq("Math").map { k =>
    SymbolMapId(k, 0) -> TypeInfo.target(ClassType(k))// TODO: special handling for global symbols
  }.toMap

  val stdLibraryMembers = numberMath.map(k => MemberId("Math", k) -> number).toMap

  lazy val std: SymbolTypes = SymbolTypes(stdLibraries, stdLibraryMembers.mapValues(TypeInfo.target))

  lazy val stdClassInfo: ClassInfo = ClassInfo(stdLibraryMembers.keySet, Map.empty)

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
  def nonEmpty = source != AnyType || target != NoType

  //assert(source == AnyType || target == NoType)
  // source should be more specific than a target (target is a supertype, source a subtype)
  // ... target is an upper bound, source a lower bound
  //assert(typeIntersect(source, target) == source)
  //assert(typeUnion(source, target) == target)
  def declType: TypeDesc = target match {
    case NoType => source
    case _ => target
  }

  def sourceTypeFromTarget: TypeDesc = target match {
    case NoType => AnyType
    case AnyType => NoType
    case _ => target
  }

  def sourceFromTarget: TypeInfo = TypeInfo(sourceTypeFromTarget, NoType)
}

case class SymbolTypes(types: Map[SymbolMapId, TypeInfo], members: Map[MemberId, TypeInfo]) {

  def get(id: Option[SymbolMapId]): Option[TypeInfo] = id.flatMap(types.get)

  def getMember(clsId: Option[MemberId]): Option[TypeInfo] = clsId.flatMap(members.get)
  def getMember(cls: Option[String], member: String): Option[TypeInfo] = getMember(cls.map(MemberId(_, member)))

  def getAsScala(id: Option[SymbolMapId]): String = {
    get(id).fold (any.toString) (t => mapSimpleTypeToScala(t.declType))
  }

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(types ++ that.types, members ++ that.members)

  def + (kv: (Option[SymbolMapId], TypeInfo)): SymbolTypes = {
    kv._1.fold(this) { id =>
      copy(types = types + (id -> kv._2))
    }
  }

  def addMember (kv: (Option[MemberId], TypeInfo)): SymbolTypes = {
    kv._1.fold(this) { id =>
      copy(members = members + (id -> kv._2))
    }
  }

}
