package org.monadscala

import scala.language.higherKinds

abstract class Comonad[F[_]] {
  def extract[A](wa: F[A]): A

  def extend[A, B](fwab: F[A] => B, wa: F[A]): F[B]

  def extendSecond[A, B](b: B, wa: F[A]): F[B] = extend(Function.const[B, F[A]](b), wa)

  def duplicate[A](wa: F[A]): F[F[A]] = extend(identity[F[A]], wa)
}

object Comonad {
  private final class ComonadTrivialFunctorInstance[F[_]: Comonad] extends Functor[F] {
    override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = Comonad[F].extend(fab.compose(Comonad[F].extract[A]), fa)

    override def replace[A, B](a: A, fb: F[B]): F[A] = Comonad[F].extend(Function.const[A, F[B]](a), fb)
  }

  private final class ComonadTrivialApplicativeInstance[F[_]: Comonad] extends Applicative[F]()(comonadTrivialFunctorInstance(Comonad[F])) {
    override def apply[A, B](ffab: F[A => B], fa: F[A]): F[B] = Comonad[F].extend(Comonad[F].extract(ffab).compose(Comonad[F].extract[A]), fa)

    override def applyFirst[A, B](fb: F[B], fa: F[A]): F[B] = Comonad[F].extend((Function.const[B, A](Comonad[F].extract(fb))(_)).compose(Comonad[F].extract[A]), fa)

    override def applySecond[A, B](fa: F[A], fb: F[B]): F[B] = Comonad[F].extend((Function.const[B, A](Comonad[F].extract(fb))(_)).compose(Comonad[F].extract[A]), fa)
  }

  def apply[F[_]: Comonad]: Comonad[F] = implicitly[Comonad[F]]

  def comonadTrivialFunctorInstance[F[_]: Comonad]: Functor[F] = new ComonadTrivialFunctorInstance()

  def comonadTrivialApplicativeInstance[F[_]: Comonad]: Applicative[F] = new ComonadTrivialApplicativeInstance()

  def liftW[F[_]: Comonad, A, B](fab: A => B, wa: F[A]): F[B] =
    Comonad[F].extend((fab(_: A)).compose(Comonad[F].extract[A]), wa)

  def liftW2[F[_]: Comonad, A, B, C](fabc: A => B => C, wa: F[A], wb: F[B]): F[C] =
    Comonad[F].extend((fabc(_: A)(Comonad[F].extract(wb))).compose(Comonad[F].extract[A]), wa)

  def liftW3[F[_]: Comonad, A, B, C, D](fabcd: A => B => C => D, wa: F[A], wb: F[B], wc: F[C]): F[D] =
    Comonad[F].extend((fabcd(_: A)(Comonad[F].extract(wb))(Comonad[F].extract(wc))).compose(Comonad[F].extract[A]), wa)
}
