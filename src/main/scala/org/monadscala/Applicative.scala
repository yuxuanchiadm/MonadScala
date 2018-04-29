package org.monadscala

import scala.language.higherKinds

abstract class Applicative[F[_]] {
  def pure[A](a: A): F[A]

  def apply[A, B](ffab: F[A => B], fa: F[A]): F[B]

  def applySecond[A, B](fa: F[A], fb: F[B]): F[B] = apply(apply(pure(Function.const(identity(_: B))(_: A)), fa), fb)

  def applyFirst[A, B](fa: F[A], fb: F[B]): F[A] = apply(apply(pure(Function.const(identity(_: A))(_: B)), fb), fa)
}

object Applicative {
  private final class ApplicativeTrivialFunctorInstance[F[_]: Applicative] extends Functor[F] {
    override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = Applicative[F].apply(Applicative[F].pure(fab), fa)

    override def replace[A, B](a: A, fb: F[B]): F[A] = Applicative[F].apply(Applicative[F].pure(Function.const(a)(_: B)), fb)
  }

  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  def applicativeTrivialFunctorInstance[F[_]: Applicative]: Functor[F] = new ApplicativeTrivialFunctorInstance()
}
