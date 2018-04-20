package org.monadscala

import scala.language.higherKinds

object Typelevel {
  type Currying[T[_, _], A] = { type Type[B] = T[A, B] }

  type Union[A, B] = { type Type[T] = ((T => Nothing) => Nothing) <:< (((A => Nothing) with (B => Nothing)) => Nothing) }
}