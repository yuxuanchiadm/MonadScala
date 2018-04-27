package org.monadscala

import scala.language.implicitConversions

object Option {
  private final class OptionSingleton0 extends MonadPlus[Option] {
    override final def unit[A](a: A): Option[A] = Some(a)

    override final def compose[A, B](ma: Option[A], famb: A => Option[B]): Option[B] = ma.flatMap(famb)

    override final def mzero[A](): Option[A] = None

    override final def mplus[A](a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }

  private final class OptionSingleton1[A](implicit constraint0: Semigroup[A]) extends Monoid[Option[A]] {
    override final def mempty(): Option[A] = None

    override final def mappend(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (None, b) => b
      case (a, None) => a
      case (Some(a), Some(b)) => Some(constraint0.assoc(a, b))
    }
  }

  implicit def optionSingleton0: MonadPlus[Option] = new OptionSingleton0()

  implicit def optionSingleton1[A](implicit constraint0: Semigroup[A]): Monoid[Option[A]] = new OptionSingleton1[A]()

  implicit def optionForNotation[A](ma: Option[A]) = Monad.forNotation[Option, A](ma)

  def none[A]: Option[A] = None

  def some[A](a: A): Option[A] = Some(a)

  def empty[A]: Option[A] = scala.Option.empty

  def apply[A](x: A): Option[A] = scala.Option.apply(x)
}
