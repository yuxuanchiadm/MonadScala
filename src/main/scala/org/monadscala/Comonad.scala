package org.monadscala

import scala.language.higherKinds

trait Comonad[F[_]] extends Functor[F] {
  override def fmap[A, B](fab: A => B, fa: F[A]): F[B] = extend((wa: F[A]) => fab(extract(wa)), fa)

  def extract[A](wa: F[A]): A

  def extend[A, B](fawb: F[A] => B, wa: F[A]): F[B]

  def duplicate[A](wa: F[A]): F[F[A]] = extend(identity[F[A]], wa)
}

object Comonad {
  def apply[F[_]](implicit f: Comonad[F]): Comonad[F] = f
}
