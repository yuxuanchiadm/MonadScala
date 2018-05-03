package org.monadscala

import scala.language.higherKinds

abstract class Functor[F[_]] {
  def fmap[A, B](fab: A => B, fa: F[A]): F[B]

  def replace[A, B](a: A, fb: F[B]): F[A] = fmap(Function.const(a), fb)
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

  implicit final class `<$`[F[_]: Functor, A, B](a: A) { def `<$`(fb: F[B]): F[A] = Functor[F].replace(a, fb) }

  implicit final class `$>`[F[_]: Functor, A, B](fa: F[A]) { def `$>`(b: B): F[B] = Functor[F].replace(b, fa) }

  implicit final class `<$>`[F[_]: Functor, A, B](fab: A => B) { def `<$>`(fa: F[A]): F[B] = Functor[F].fmap(fab, fa) }

  implicit final class <&>[F[_]: Functor, A, B](fa: F[A]) { def <&>(fab: A => B): F[B] = Functor[F].fmap(fab, fa) }

  def void[F[_]: Functor, A](fa: F[A]): F[Unit] = Functor[F].replace((), fa)
}
