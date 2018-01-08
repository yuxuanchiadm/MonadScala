package org.monadscala

import scala.language.higherKinds;

trait Comonad[F[_]] extends Functor[F] {
  def extract[A](wa: F[A]): A;

  def extend[A, B](wa: F[A], fawb: F[A] => B): F[B] = fmap(fawb, duplicate(wa));

  def duplicate[A](wa: F[A]): F[F[A]] = extend(wa, identity[F[A]]);
}

object Comonad {
  def apply[F[_]](implicit f: Comonad[F]): Comonad[F] = f;
}
