package org.monadscala

import scala.language.higherKinds

abstract class Monad[F[_]] {
  def unit[A](a: A): F[A]

  def compose[A, B](ma: F[A], famb: A => F[B]): F[B]

  def composeSecond[A, B](fa: F[A], fb: F[B]): F[B] = compose(fa, Function.const(fb))
}

object Monad {
  private final class MonadForNotation[F[_]: Monad, A](ma: F[A]) extends ForNotation[F, A] {
    override def flatMap[B](famb: A => F[B]): F[B] = Monad[F].compose(ma, famb)

    override def map[B](fab: A => B): F[B] = Monad[F].compose(ma, (a: A) => Monad[F].unit(fab(a)))
  }

  private final class MonadTrivialFunctorInstance[F[_]: Monad] extends Functor[F] {
    override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = Monad[F].compose(fa, (a: A) => Monad[F].unit(fab(a)))

    override def replace[A, B](a: A, fb: F[B]): F[A] = Monad[F].compose(fb, Function.const(Monad[F].unit(a)))
  }

  private final class MonadTrivialApplicativeInstance[F[_]: Monad] extends Applicative[F] {
    override def pure[A](a: A): F[A] = Monad[F].unit(a)

    override def apply[A, B](ffab: F[A => B], fa: F[A]): F[B] = Monad[F].compose(ffab, (fab: A => B) => Monad[F].compose(fa, (a: A) => Monad[F].unit(fab.apply(a))))

    override def applySecond[A, B](fa: F[A], fb: F[B]): F[B] = Monad[F].compose(fa, Function.const(fb))

    override def applyFirst[A, B](fa: F[A], fb: F[B]): F[A] = Monad[F].compose(fb, Function.const(fa))
  }

  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  def forNotation[F[_]: Monad, A](ma: F[A]): ForNotation[F, A] = new MonadForNotation(ma)

  def monadTrivialFunctorInstance[F[_]: Monad]: Functor[F] = new MonadTrivialFunctorInstance()

  def monadTrivialApplicativeInstance[F[_]: Monad]: Applicative[F] = new MonadTrivialApplicativeInstance()

  implicit final class >>=[F[_]: Monad, A, B](ma: F[A]) { def >>=(famb: A => F[B]): F[B] = Monad[F].compose(ma, famb) }

  implicit final class >>[F[_]: Monad, A, B](ma: F[A]) { def >>(mb: F[B]): F[B] = Monad[F].composeSecond(ma, mb) }

  def liftM[F[_]: Monad, A, B](fab: A => B, ma: F[A]): F[B] =
    Monad[F].compose(ma, (a: A) => Monad[F].unit(fab(a)))

  def liftM2[F[_]: Monad, A, B, C](fabc: A => B => C, ma: F[A], mb: F[B]): F[C] =
    Monad[F].compose(ma, (a: A) => Monad[F].compose(mb, (b: B) => Monad[F].unit(fabc(a)(b))))

  def liftM3[F[_]: Monad, A, B, C, D](fabc: A => B => C => D, ma: F[A], mb: F[B], mc: F[C]): F[D] =
    Monad[F].compose(ma, (a: A) => Monad[F].compose(mb, (b: B) => Monad[F].compose(mc, (c: C) => Monad[F].unit(fabc(a)(b)(c)))))
}
