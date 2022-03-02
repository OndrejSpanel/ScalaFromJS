package com.github.opengrabeso.scalafromjs

// provide comparison based on object instance equality instead of value equals
class IdentityBox[T <: AnyRef](val value: T) {
  override def equals(other: Any): Boolean = other match {
    case that: IdentityBox[T@unchecked] => that.value eq this.value
    case _ => false
  }

  override def hashCode(): Int = System.identityHashCode(value)
}