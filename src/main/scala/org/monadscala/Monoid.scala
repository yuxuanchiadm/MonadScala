package org.monadscala

trait Monoid[A] extends Semigroup[A] {
  override def op(a1: A, a2: A): A = mappend(a1, a2)

  override def assoc(a1: A, a2: A): A = mappend(a1, a2)

  def mempty(): A

  def mappend(a1: A, a2: A): A

  def mconcat(t: Traversable[A]): A = (mempty() /: t)(mappend(_, _))
}

object Monoid {
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}
