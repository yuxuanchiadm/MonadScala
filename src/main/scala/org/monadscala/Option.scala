package org.monadscala

import scala.language.implicitConversions

object Option {
  private final class OptionTrivialMonadInstance extends Monad[Option] {
    override final def unit[A](a: A): Option[A] = Some(a)

    override final def compose[A, B](ma: Option[A], famb: A => Option[B]): Option[B] = ma.flatMap(famb)
  }

  private final class OptionTrivialMonadPlusInstance extends MonadPlus[Option] {
    override final def mzero[A](): Option[A] = None

    override final def mplus[A](a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }

  private final class OptionTrivialMonoidInstance[A: Semigroup] extends Monoid[Option[A]] {
    override final def mempty(): Option[A] = None

    override final def mappend(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (None, b) => b
      case (a, None) => a
      case (Some(a), Some(b)) => Some(Semigroup[A].assoc(a, b))
    }
  }

  private final class OptionFailMonoidInstance[A: Monoid] extends Monoid[Option[A]] {
    override final def mempty(): Option[A] = Some(Monoid[A].mempty())

    override final def mappend(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (Some(a), Some(b)) => Some(Monoid[A].mappend(a, b))
      case _ => None
    }
  }

  implicit def optionTrivialFunctorInstance: Functor[Option] = Monad.monadTrivialFunctorInstance[Option]

  implicit def optionTrivialApplicativeInstance: Applicative[Option] = Monad.monadTrivialApplicativeInstance[Option]

  implicit def optionTrivialMonadInstance: Monad[Option] = new OptionTrivialMonadInstance()

  implicit def optionTrivialAlternativeInstance: Alternative[Option] = MonadPlus.monadPlusTrivialAlternativeInstance[Option]

  implicit def optionTrivialMonadPlusInstance: MonadPlus[Option] = new OptionTrivialMonadPlusInstance()

  implicit def optionTrivialMagmaInstance[A: Semigroup]: Magma[Option[A]] = Monoid.monoidTrivialMagmaInstance[Option[A]]

  implicit def optionTrivialSemigroupInstance[A: Semigroup]: Semigroup[Option[A]] = Monoid.monoidTrivialSemigroupInstance[Option[A]]

  implicit def optionTrivialMonoidInstance[A: Semigroup]: Monoid[Option[A]] = new OptionTrivialMonoidInstance[A]()

  def optionFailMagmaInstance[A: Monoid]: Magma[Option[A]] = Monoid.monoidTrivialMagmaInstance[Option[A]](optionFailMonoidInstance)

  def optionFailSemigroupInstance[A: Monoid]: Semigroup[Option[A]] = Monoid.monoidTrivialSemigroupInstance[Option[A]](optionFailMonoidInstance)

  def optionFailMonoidInstance[A: Monoid]: Monoid[Option[A]] = new OptionFailMonoidInstance[A]()

  implicit def optionForNotation[A](ma: Option[A]) = Monad.forNotation[Option, A](ma)

  def none[A]: Option[A] = None

  def some[A](a: A): Option[A] = Some(a)

  def empty[A]: Option[A] = scala.Option.empty

  def apply[A](x: A): Option[A] = scala.Option.apply(x)
}
