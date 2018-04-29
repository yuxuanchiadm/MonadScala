package org.monadscala

import scala.language.higherKinds

abstract class MonadPlus[F[_]: Monad] {
  def monadInstance(): Monad[F] = Monad[F]

  def mzero[A](): F[A]

  def mplus[A](a1: F[A], a2: F[A]): F[A]
}

object MonadPlus {
  private final class MonadPlusTrivialAlternativeInstance[F[_]: MonadPlus] extends Alternative[F]()(Monad.monadTrivialApplicativeInstance(MonadPlus[F].monadInstance())) {
    override def empty[A](): F[A] = MonadPlus[F].mzero()

    override def combine[A](a1: F[A], a2: F[A]): F[A] = MonadPlus[F].mplus(a1, a2)
  }

  def apply[F[_]: MonadPlus]: MonadPlus[F] = implicitly[MonadPlus[F]]

  def monadPlusTrivialAlternativeInstance[F[_]: MonadPlus]: Alternative[F] = new MonadPlusTrivialAlternativeInstance()
}
