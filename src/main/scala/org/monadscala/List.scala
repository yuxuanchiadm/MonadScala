package org.monadscala

import scala.language.implicitConversions

object List {
  private final class ListSingleton0 extends MonadPlus[List] {
    override final def unit[A](a: A): List[A] = a :: Nil

    override final def compose[A, B](ma: List[A], famb: A => List[B]): List[B] = ma.flatMap(famb)

    override final def mzero[A](): List[A] = Nil

    override final def mplus[A](a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  private final class ListSingleton1[A] extends Monoid[List[A]] {
    override final def mempty(): List[A] = Nil

    override final def mappend(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  implicit def listSingleton0: MonadPlus[List] = new ListSingleton0()

  implicit def listSingleton1[A]: Monoid[List[A]] = new ListSingleton1[A]()

  implicit def listForNotation[A](ma: List[A]) = Monad.forNotation[List, A](ma)

  def empty[A]: List[A] = scala.collection.immutable.List.empty

  def apply[A](xs: A*): List[A] = scala.collection.immutable.List.apply(xs: _*)
}
