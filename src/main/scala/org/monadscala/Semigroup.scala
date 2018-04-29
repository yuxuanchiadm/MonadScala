package org.monadscala

abstract class Semigroup[A] {
  def assoc(a1: A, a2: A): A
}

object Semigroup {
  private final class SemigroupTrivialMagmaInstance[A: Semigroup] extends Magma[A] {
    override def op(a1: A, a2: A): A = Semigroup[A].assoc(a1, a2)
  }

  def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]

  def semigroupTrivialMagmaInstance[A: Semigroup]: Magma[A] = new SemigroupTrivialMagmaInstance()
}
