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
  case class FunctionType(ret: Option[TypeDesc], args: IndexedSeq[Option[TypeDesc]]) extends TypeDesc {
    override def toString = {
      def outputType(o: Option[TypeDesc]) = o.fold("Any")(_.toString)
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }
  }

  val any = SimpleType("Any")
  val number = SimpleType("number")
  val boolean = SimpleType("boolean")
  val string = SimpleType("string")

  /* it would be tempting to use something like _! to avoid possible clashes with other identifiers
  That would hover require to always add a traling space or to use `around the symbol` to prevent any following operator
  be considered a part of the identifier, as in name_!: Any
  */
  val parSuffix = "_par"


  def parseType(str: String): TypeDesc = SimpleType(str)

  // SymbolDef instances (including ids) are recreated on each figure_out_scope
  // we need a stable id. Original source location + name should be unique and stable
  case class SymbolMapId(name: String, sourcePos: Int)

  case class MemberId(cls: String, name: String)

  def memberId(maybeDesc: Option[String], name: String): Option[MemberId] = {
    maybeDesc.map(MemberId(_, name))
  }

  implicit def id(sym: SymbolDef): Option[SymbolMapId] = {
    val token = sym.orig.headOption.flatMap { _.start.nonNull }
    token.map(t => SymbolMapId(sym.name, t.pos))
  }

  def mapSimpleTypeToScala(tpe: TypeDesc): String = {
    tpe match {
      case `string` => "String"
      case `number` => "Double"
      case `boolean` => "Boolean"
      case _ => tpe.toString
    }
  }

  def classFromType(tpe: Option[TypeDesc]): Option[String] = {
    tpe match {
      case Some(ClassType(name)) => Some(name)
      case _ => None
    }
  }

  def typeUnion(tpe1: TypeDesc, tpe2: TypeDesc) = {
    if (tpe2 == tpe1) tpe1 else any
  }

  def typeUnionOption(tpe1: Option[TypeDesc], tpe2: Option[TypeDesc]): Option[TypeDesc] = {
    (tpe1, tpe2) match {
      case (_, None) => tpe1
      case (None, _) => tpe2
      case (Some(t1), Some(t2)) => Some(typeUnion(t1, t2))
      case _ => None
    }
  }

  def apply(): SymbolTypes = new SymbolTypes(Map.empty, Map.empty)
  def apply(syms: Seq[(SymbolDef, TypeDesc)]) = {
    val idMap = syms.map { case (k,v) => id(k) -> v }.toMap - None
    new SymbolTypes(idMap.map{ case (k, v) => k.get -> v}, Map.empty)
  }

}

import SymbolTypes._

case class SymbolTypes(types: Map[SymbolMapId, TypeDesc], members: Map[MemberId, TypeDesc]) {

  def setOfTypes: Set[TypeDesc] = types.values.toSet

  def get(id: Option[SymbolMapId]): Option[TypeDesc] = id.flatMap(types.get)

  def getMember(clsId: Option[MemberId]): Option[TypeDesc] = clsId.flatMap(members.get)
  def getMember(cls: Option[String], member: String): Option[TypeDesc] = getMember(cls.map(MemberId(_, member)))

  def getAsScala(id: Option[SymbolMapId]): String = {
    get(id).fold (any.toString) (mapSimpleTypeToScala)
  }

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(types ++ that.types, members ++ that.members)

  def + (kv: (Option[SymbolMapId], TypeDesc)): SymbolTypes = {
    kv._1.fold(this) { id =>
      copy(types = types + (id -> kv._2))
    }
  }

  def addMember (kv: (Option[MemberId], TypeDesc)): SymbolTypes = {
    kv._1.fold(this) { id =>
      copy(members = members + (id -> kv._2))
    }
  }

}
