package org.monadscala

import scala.language.higherKinds;

trait Alternative[F[_]] extends Applicative[F] {
  def empty[A]: F[A];

  def combine[A](a1: F[A], a2: F[A]): F[A];
}

object Alternative {
  def apply[F[_]](implicit f: Alternative[F]): Alternative[F] = f;
}
