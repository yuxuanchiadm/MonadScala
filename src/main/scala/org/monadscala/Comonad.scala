package org.monadscala

import scala.language.higherKinds

abstract class Comonad[F[_]] {
  def extract[A](wa: F[A]): A

  def extend[A, B](fwab: F[A] => B, wa: F[A]): F[B]

  def duplicate[A](wa: F[A]): F[F[A]] = extend(identity[F[A]], wa)

  def extendSecond[A, B](b: B, wa: F[A]): F[B] = extend(Function.const[B, F[A]](b), wa)
}

object Comonad {
  private final class ComonadTrivialFunctorInstance[F[_]: Comonad] extends Functor[F] {
    override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = Comonad[F].extend((wa: F[A]) => fab(Comonad[F].extract(wa)), fa)

    override def replace[A, B](a: A, fb: F[B]): F[A] = Comonad[F].extend(Function.const(a)(_: F[B]), fb)
  }

  def apply[F[_]: Comonad]: Comonad[F] = implicitly[Comonad[F]]

  def comonadTrivialFunctorInstance[F[_]: Comonad]: Functor[F] = new ComonadTrivialFunctorInstance()
}
