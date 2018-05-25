package org.monadscala.typeclass

abstract class Monoid[A] {
  def mempty(): A

  def mappend(a1: A, a2: A): A

  def mconcat(t: Traversable[A]): A = t.foldLeft(mempty())(mappend)
}

object Monoid {
  private final class MonoidTrivialMagmaInstance[A: Monoid] extends Magma[A] {
    override def op(a1: A, a2: A): A = Monoid[A].mappend(a1, a2)
  }

  private final class MonoidTrivialSemigroupInstance[A: Monoid] extends Semigroup[A] {
    override def assoc(a1: A, a2: A): A = Monoid[A].mappend(a1, a2)
  }

  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

  def monoidTrivialMagmaInstance[A: Monoid]: Magma[A] = new MonoidTrivialMagmaInstance()

  def monoidTrivialSemigroupInstance[A: Monoid]: Semigroup[A] = new MonoidTrivialSemigroupInstance()
}
