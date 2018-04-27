package org.monadscala

trait Semigroup[A] extends Magma[A] {
  override def op(a1: A, a2: A): A = assoc(a1, a2)

  def assoc(a1: A, a2: A): A
}

object Semigroup {
  def apply[A](implicit m: Semigroup[A]): Semigroup[A] = m
}
