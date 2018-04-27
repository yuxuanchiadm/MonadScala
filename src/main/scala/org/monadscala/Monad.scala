package org.monadscala

import scala.language.higherKinds

trait Monad[F[_]] extends Applicative[F] {
  override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = compose(fa, (a: A) => unit(fab(a)))

  override def replace[A, B](a: A, fb: F[B]): F[A] = compose(fb, Function.const(unit(a)))

  override def pure[A](a: A): F[A] = unit(a)

  override def apply[A, B](ffab: F[A => B], fa: F[A]): F[B] = compose(ffab, (fab: A => B) => compose(fa, (a: A) => unit(fab.apply(a))))

  override def applySecond[A, B](fa: F[A], fb: F[B]): F[B] = compose(fa, Function.const(fb))

  override def applyFirst[A, B](fa: F[A], fb: F[B]): F[A] = compose(fb, Function.const(fa))

  def unit[A](a: A): F[A]

  def compose[A, B](ma: F[A], famb: A => F[B]): F[B]

  final def composeSecond[A, B](fa: F[A], fb: F[B]): F[B] = compose(fa, Function.const(fb))
}

object Monad {
  def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

  def forNotation[F[_], A](ma: F[A])(implicit m: Monad[F]): ForNotation[F, A] = new ForNotation[F, A] {
    override def flatMap[B](famb: A => F[B]): F[B] = m.compose(ma, famb)

    override def map[B](fab: A => B): F[B] = m.compose(ma, (a: A) => m.unit(fab(a)))
  }

  sealed trait ForNotation[F[_], A] {
    def flatMap[B](famb: A => F[B]): F[B]

    def map[B](fab: A => B): F[B]
  }
}
