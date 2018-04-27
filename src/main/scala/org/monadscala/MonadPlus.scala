package org.monadscala

import scala.language.higherKinds

trait MonadPlus[F[_]] extends Alternative[F] with Monad[F] {
  override def empty[A](): F[A] = mzero()

  override def combine[A](a1: F[A], a2: F[A]): F[A] = mplus(a1, a2)

  def mzero[A](): F[A]

  def mplus[A](a1: F[A], a2: F[A]): F[A]
}

object MonadPlus {
  def apply[F[_]](implicit f: MonadPlus[F]): MonadPlus[F] = f
}
