package org.monadscala

import scala.language.higherKinds

object Typelevel {
  type Curry0[F] = {
    type <| = F
  }

  type Curry1[F[_]] = {
    type <|[A] = F[A]
    type <[A] = {
      type <| = F[A]
    }
  }

  type Curry2[F[_, _]] = {
    type <|[A, B] = F[A, B]
    type <[A] = {
      type <|[B] = F[A, B]
      type <[B] = {
        type <| = F[A, B]
      }
    }
  }

  type Curry3[F[_, _, _]] = {
    type <|[A, B, C] = F[A, B, C]
    type <[A] = {
      type <|[B, C] = F[A, B, C]
      type <[B] = {
        type <|[C] = F[A, B, C]
        type <[C] = {
          type <| = F[A, B, C]
        }
      }
    }
  }

  type Curry4[F[_, _, _, _]] = {
    type <|[A, B, C, D] = F[A, B, C, D]
    type <[A] = {
      type <|[B, C, D] = F[A, B, C, D]
      type <[B] = {
        type <|[C, D] = F[A, B, C, D]
        type <[C] = {
          type <|[D] = F[A, B, C, D]
          type <[D] = {
            type <| = F[A, B, C, D]
          }
        }
      }
    }
  }

  type Id[A] = A

  type Not[A] = A => Nothing

  type Union[A, B] = { type Ev[C] = Not[Not[C]] <:< Not[Not[A] with Not[B]] }
  
  sealed case class TTerm[A]()
}
