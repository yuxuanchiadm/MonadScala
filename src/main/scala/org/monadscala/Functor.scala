package org.monadscala

import scala.language.higherKinds;

trait Functor[F[_]] {
  def fmap[A, B](fab: A => B, fa: F[A]): F[B];

  def replace[A, B](a: A, fb: F[B]): F[A] = fmap(Function.const(a), fb);
}

object Functor {
  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f;
}
