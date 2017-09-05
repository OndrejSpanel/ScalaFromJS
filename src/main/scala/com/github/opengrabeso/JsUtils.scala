package com.github.opengrabeso

import scala.reflect.ClassTag
import scala.scalajs.js
import ContainerHelpers._

object JsUtils {
  implicit class NonNull[T](val undef: js.UndefOr[T])(implicit ev: Null <:< T) {
    def nonNull: Option[T] = Option[T](undef.orNull)
  }

  implicit class JsArrayOps[X](a: js.Array[X]) {
    def partitionByType[T <: X: ClassTag]: (Traversable[T], Traversable[X]) = a.toTraversable.partitionByType[T]
  }
}
