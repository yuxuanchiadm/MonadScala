package org.monadscala

import scala.language.higherKinds

abstract class Semigroupoid[G[_, _]] {
  def assoc[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C]
}

object Semigroupoid {
  def apply[G[_, _]: Semigroupoid]: Semigroupoid[G] = implicitly[Semigroupoid[G]]
}
