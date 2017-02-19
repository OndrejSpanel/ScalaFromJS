package com.github.opengrabeso

import scala.scalajs.js

object JsUtils {
  implicit class NonNull[T](val undef: js.UndefOr[T])(implicit ev: Null <:< T) {
    def nonNull: Option[T] = Option[T](undef.orNull)
  }
}
