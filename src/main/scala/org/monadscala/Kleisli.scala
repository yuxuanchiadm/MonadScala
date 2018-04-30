package org.monadscala

import scala.language.higherKinds

sealed case class Kleisli[M[_], A, B](runKleisli: A => M[B])

object Kleisli {
  type Kleisli$1[M[_]] = { type Type[A, B] = Kleisli[M, A, B] }

  type Kleisli$2[M[_], A] = { type Type[B] = Kleisli[M, A, B] }

  private final class KleisliTrivialCategoryInstance[M[_]: Monad] extends Category[Kleisli$1[M]#Type] {
    override final def catid[A](): Kleisli[M, A, A] = Kleisli(Monad[M].unit)

    override final def catmor[A, B, C](gbc: Kleisli[M, B, C], gab: Kleisli[M, A, B]): Kleisli[M, A, C] = Kleisli(a => Monad[M].compose(gab.runKleisli(a), gbc.runKleisli))
  }

  implicit def kleisliTrivialSemigroupoidInstance[M[_]: Monad]: Semigroupoid[Kleisli$1[M]#Type] = Category.categoryTrivialSemigroupoidInstance[Kleisli$1[M]#Type]

  implicit def kleisliTrivialCategoryInstance[M[_]: Monad]: Category[Kleisli$1[M]#Type] = new KleisliTrivialCategoryInstance[M]()
}
