package com.github.opengrabeso

import Uglify.SymbolDef


object SymbolTypes {
  type TypeDesc = String

  def apply(): SymbolTypes = SymbolTypes(Map.empty)
}

case class SymbolTypes(types: Map[SymbolDef, SymbolTypes.TypeDesc]) {
  def apply(sym: SymbolDef): SymbolTypes.TypeDesc = types(sym)

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(types ++ that.types)
}
