package org.monadscala.typeclass

import scala.language.higherKinds

abstract class Alternative[F[_]: Functor] {
  def functorInstance(): Functor[F] = Functor[F]

  def empty[A](): F[A]

  def combine[A](fa1: F[A], fa2: F[A]): F[A]
}

object Alternative {
  def apply[F[_]: Alternative]: Alternative[F] = implicitly[Alternative[F]]

  implicit final class <|>[F[_]: Alternative, A](fa1: F[A]) { def >>=(fa2: F[A]): F[A] = Alternative[F].combine(fa1, fa2) }
}
