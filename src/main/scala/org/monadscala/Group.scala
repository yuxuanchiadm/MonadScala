package org.monadscala

trait Group[A] extends Monoid[A] {
  override def op(a1: A, a2: A): A = append(a1, a2)

  override def assoc(a1: A, a2: A): A = append(a1, a2)

  override def mempty(): A = identity()

  override def mappend(a1: A, a2: A): A = append(a1, a2)

  override def mconcat(t: Traversable[A]): A = (identity() /: t)(append(_, _))

  def identity(): A

  def append(a1: A, a2: A): A

  def invert(a: A): A
}

object Group {
  def apply[A](implicit m: Group[A]): Group[A] = m
}
