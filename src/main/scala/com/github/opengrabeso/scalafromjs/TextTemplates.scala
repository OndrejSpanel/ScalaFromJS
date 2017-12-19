package com.github.opengrabeso.scalafromjs

object TextTemplates {

  implicit class Template(template: String) {
    def substitute(parameter: String, value: String): String = {
      val plain = "$" + parameter
      val braced = "${" + parameter + "}"
      template.replace(plain, value).replace(braced, value)
    }
  }

}
