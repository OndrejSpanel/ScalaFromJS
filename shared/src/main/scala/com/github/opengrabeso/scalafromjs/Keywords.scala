package com.github.opengrabeso.scalafromjs

object Keywords {
  private val all = Set(
    // this is used with the same meaning in JS and Scala - we cannot escape it
    "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome",
    "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private",
    "protected", "return", "sealed", "super", /*"this", */ "throw", "trait", "try", "true", "type", "val",
    "var", "while", "with", "yield"
  )
  def apply(name: String) = all.contains(name)
}
