package com.github.opengrabeso

import scala.reflect.ClassTag

object ContainerHelpers {

  implicit class TraversableOps[X](a: Traversable[X]) {
    def partitionByType[T <: X: ClassTag]: (Traversable[T], Traversable[X]) = {
      val (typed, other) = a.partition {
        case _: T => true
        case _ => false
      }
      (typed.map(_.asInstanceOf[T]), other)
    }

  }
}
