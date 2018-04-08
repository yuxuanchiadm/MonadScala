package org.monadscala

import scala.language.higherKinds;

object Typelevel {
  type Currying[T[_, _], A] = { type Type[B] = T[A, B] }
}