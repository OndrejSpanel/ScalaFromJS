package com.github.opengrabeso

trait Symbols {
  final val asinstanceof = "asinstanceof"
  final val instanceof = "instanceof"

  /* it would be tempting to use something like _! to avoid possible clashes with other identifiers
  That would hover require to always add a traling space or to use `around the symbol` to prevent any following operator
  be considered a part of the identifier, as in name_!: Any
  */
  final val parSuffix = "_par"
  final val castSuffix = "_cast"

}


object Symbols extends Symbols
