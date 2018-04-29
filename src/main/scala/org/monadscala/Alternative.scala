package org.monadscala

import scala.language.higherKinds

abstract class Alternative[F[_]: Applicative] {
  def applicativeInstance(): Applicative[F] = Applicative[F]

  def empty[A](): F[A]

  def combine[A](a1: F[A], a2: F[A]): F[A]
}

object Alternative {
  def apply[F[_]: Alternative]: Alternative[F] = implicitly[Alternative[F]]
}
