package org.monadscala

import scala.language.higherKinds

abstract class Functor[F[_]] {
  def fmap[A, B](fab: A => B, fa: F[A]): F[B]

  def replace[A, B](a: A, fb: F[B]): F[A] = fmap(Function.const(a), fb)
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}
