package org.monadscala.instance

import org.monadscala.typeclass._

import scala.language.implicitConversions

object List {
  private final class ListTrivialMonadInstance extends Monad[List] {
    override final def unit[A](a: A): List[A] = a :: Nil

    override final def compose[A, B](ma: List[A], famb: A => List[B]): List[B] = ma.flatMap(famb)
  }

  private final class ListTrivialAlternativeInstance extends Alternative[List] {
    override final def empty[A](): List[A] = Nil

    override final def combine[A](fa1: List[A], fa2: List[A]): List[A] = fa1 ++ fa2
  }

  private final class ListTrivialMonoidInstance[A] extends Monoid[List[A]] {
    override final def mempty(): List[A] = Nil

    override final def mappend(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  implicit def listTrivialFunctorInstance: Functor[List] = Monad.monadTrivialFunctorInstance[List]

  implicit def listTrivialApplicativeInstance: Applicative[List] = Monad.monadTrivialApplicativeInstance[List]

  implicit def listTrivialMonadInstance: Monad[List] = new ListTrivialMonadInstance()

  implicit def listTrivialAlternativeInstance: Alternative[List] = new ListTrivialAlternativeInstance()

  implicit def listTrivialMagmaInstance[A]: Magma[List[A]] = Monoid.monoidTrivialMagmaInstance[List[A]]

  implicit def listTrivialSemigroupInstance[A]: Semigroup[List[A]] = Monoid.monoidTrivialSemigroupInstance[List[A]]

  implicit def listTrivialMonoidInstance[A]: Monoid[List[A]] = new ListTrivialMonoidInstance[A]()

  implicit def listForNotation[A](ma: List[A]) = Monad.forNotation[List, A](ma)

  def empty[A]: List[A] = scala.collection.immutable.List.empty

  def apply[A](xs: A*): List[A] = scala.collection.immutable.List.apply(xs: _*)
}
