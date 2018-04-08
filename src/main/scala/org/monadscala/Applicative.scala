package org.monadscala

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = apply(pure(fab), fa)

  override def replace[A, B](a: A, fb: F[B]): F[A] = apply(pure(Function.const(a)(_: B)), fb)

  def pure[A](a: A): F[A]

  def apply[A, B](ffab: F[A => B], fa: F[A]): F[B]

  def applySecond[A, B](fa: F[A], fb: F[B]): F[B] = apply(replace(identity(_: B), fa), fb)

  def applyFirst[A, B](fa: F[A], fb: F[B]): F[A] = apply(replace(identity(_: A), fb), fa)
}

object Applicative {
  def apply[F[_]](implicit f: Applicative[F]): Applicative[F] = f
}
