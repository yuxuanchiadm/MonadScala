package org.monadscala.typeclass

abstract class Magma[A] {
  def op(a1: A, a2: A): A
}

object Magma {
  def apply[A: Magma]: Magma[A] = implicitly[Magma[A]]
}
