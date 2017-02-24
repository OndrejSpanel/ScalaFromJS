package com.github.opengrabeso

import Uglify.SymbolDef
import JsUtils._

object SymbolTypes {
  type TypeDesc = String

  val any = "Any"
  val number = "number"
  val boolean = "boolean"
  val string = "string"

  // SymbolDef instances (including ids) are recreated on each figure_out_scope
  // we need a stable id. Original source location + name should be unique and stable
  case class SymbolMapId(name: String, sourcePos: Int)

  def id(sym: SymbolDef): Option[SymbolMapId] = {
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

  def apply(): SymbolTypes = new SymbolTypes(Map.empty)
  def apply(syms: Seq[(SymbolDef, TypeDesc)]) = {
    val idMap = syms.map { case (k,v) => id(k) -> v }.toMap - None
    new SymbolTypes(idMap.map{ case (k, v) => k.get -> v})
  }

}

import SymbolTypes._

case class SymbolTypes(types: Map[SymbolMapId, TypeDesc]) {
  def setOfTypes: Set[TypeDesc] = types.values.toSet

  def apply(sym: SymbolDef): TypeDesc = types(id(sym).get)

  def get(sym: SymbolDef): Option[TypeDesc] = id(sym).flatMap(types.get)

  def getAsScala(sym: SymbolDef): String = {
    get(sym).fold (any) (mapSimpleTypeToScala)
  }

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(types ++ that.types)

  def + (kv: (SymbolDef, TypeDesc)): SymbolTypes = {
    id(kv._1).fold(this) { id =>
      SymbolTypes(types + (id -> kv._2))
    }
  }
}
