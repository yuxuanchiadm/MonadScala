package org.monadscala

import scala.language.higherKinds

abstract class Applicative[F[_]: Functor] {
  def functorInstance(): Functor[F] = Functor[F]

  def apply[A, B](ffab: F[A => B], fa: F[A]): F[B]

  def applyFirst[A, B](fb: F[B], fa: F[A]): F[B] = apply(Functor[F].fmap(Function.const[B, A], fb), fa)

  def applySecond[A, B](fa: F[A], fb: F[B]): F[B] = apply(Functor[F].fmap(Function.const[B, A], fb), fa)
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  implicit final class <*>[F[_]: Applicative, A, B](ffab: F[A => B]) { def <*>(fa: F[A]): F[B] = Applicative[F].apply(ffab, fa) }

  def liftMA[F[_]: Applicative: Monad, A, B](fab: A => B, ma: F[A]): F[B] =
    Monad[F].compose(ma, (Monad[F].unit(_: B)).compose(fab))

  def liftMA2[F[_]: Applicative: Monad, A, B, C](fabc: A => B => C, ma: F[A], mb: F[B]): F[C] =
    Applicative[F].apply(Monad[F].compose(ma, (Monad[F].unit(_: B => C)).compose(fabc)), mb)

  def liftMA3[F[_]: Applicative: Monad, A, B, C, D](fabcd: A => B => C => D, ma: F[A], mb: F[B], mc: F[C]): F[D] =
    Applicative[F].apply(Applicative[F].apply(Monad[F].compose(ma, (Monad[F].unit(_: B => C => D)).compose(fabcd)), mb), mc)

  def liftWA[F[_]: Applicative: Comonad, A, B](fab: A => B, wa: F[A]): F[B] =
    Comonad[F].extend(fab.compose(Comonad[F].extract[A]), wa)

  def liftWA2[F[_]: Applicative: Comonad, A, B, C](fabc: A => B => C, wa: F[A], wb: F[B]): F[C] =
    Applicative[F].apply(Comonad[F].extend(fabc.compose(Comonad[F].extract[A]), wa), wb)

  def liftWA3[F[_]: Applicative: Comonad, A, B, C, D](fabcd: A => B => C => D, wa: F[A], wb: F[B], wc: F[C]): F[D] =
    Applicative[F].apply(Applicative[F].apply(Comonad[F].extend(fabcd.compose(Comonad[F].extract[A]), wa), wb), wc)
}
