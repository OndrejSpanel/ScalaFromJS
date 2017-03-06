package com.github.opengrabeso

import Uglify.SymbolDef
import JsUtils._

import scala.language.implicitConversions

object SymbolTypes {

  type TypeDesc = String

  val any = "Any"
  val number = "number"
  val boolean = "boolean"
  val string = "string"

  // SymbolDef instances (including ids) are recreated on each figure_out_scope
  // we need a stable id. Original source location + name should be unique and stable
  case class SymbolMapId(name: String, sourcePos: Int)

  case class MemberId(cls: String, name: String)

  def memberId(maybeDesc: Option[TypeDesc], name: String): Option[MemberId] = {
    maybeDesc.map(MemberId(_, name))
  }

  implicit def id(sym: SymbolDef): Option[SymbolMapId] = {
    val token = sym.orig.headOption.flatMap { _.start.nonNull }
    token.map(t => SymbolMapId(sym.name, t.pos))
  }

  def mapSimpleTypeToScala(tpe: String): String = {
    tpe match {
      case `string` => "String"
      case `number` => "Double"
      case `boolean` => "Boolean"
      case _ => tpe
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

  def getAsScala(id: Option[SymbolMapId]): String = {
    get(id).fold (any) (mapSimpleTypeToScala)
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
