package org.monadscala

trait Magma[A] {
  def op(a1: A, a2: A): A
}

object Magma {
  def apply[A](implicit m: Magma[A]): Magma[A] = m
}
