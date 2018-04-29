package org.monadscala

import scala.language.higherKinds

trait ForNotation[F[_], A] {
  def flatMap[B](famb: A => F[B]): F[B]

  def map[B](fab: A => B): F[B]
}