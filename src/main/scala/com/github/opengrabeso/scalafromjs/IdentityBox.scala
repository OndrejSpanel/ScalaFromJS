package com.github.opengrabeso.scalafromjs

class IdentityBox[T <: AnyRef](val value: T) {
  override def equals(other: Any): Boolean = other match {
    case that: IdentityBox[T] => that.value eq this.value
    case _ => false
  }

  override def hashCode(): Int = System.identityHashCode(value)
}