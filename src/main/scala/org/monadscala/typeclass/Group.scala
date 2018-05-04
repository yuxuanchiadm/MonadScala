package org.monadscala.typeclass

abstract class Group[A] {
  def identity(): A

  def append(a1: A, a2: A): A

  def invert(a: A): A
}

object Group {
  private final class GroupTrivialMagmaInstance[A: Group] extends Magma[A] {
    override def op(a1: A, a2: A): A = Group[A].append(a1, a2)
  }

  private final class GroupTrivialSemigroupInstance[A: Group] extends Semigroup[A] {
    override def assoc(a1: A, a2: A): A = Group[A].append(a1, a2)
  }

  private final class GroupTrivialMonoidInstance[A: Group] extends Monoid[A] {
    override def mempty(): A = Group[A].identity()

    override def mappend(a1: A, a2: A): A = Group[A].append(a1, a2)

    override def mconcat(t: Traversable[A]): A = (Group[A].identity() /: t)(Group[A].append)
  }

  def apply[A: Group]: Group[A] = implicitly[Group[A]]

  def groupTrivialMagmaInstance[A: Group]: Magma[A] = new GroupTrivialMagmaInstance()

  def groupTrivialSemigroupInstance[A: Group]: Semigroup[A] = new GroupTrivialSemigroupInstance()

  def groupTrivialMonoidInstance[A: Group]: Monoid[A] = new GroupTrivialMonoidInstance()
}
