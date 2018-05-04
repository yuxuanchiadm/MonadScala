package org.monadscala

import scala.language.higherKinds

trait ForNotation[F[_], G[_], A] {
  def flatMap[B](famb: A => F[B]): G[B]

  def map[B](fab: A => B): G[B]
}
