package org.monadscala

import scala.language.higherKinds

sealed case class Cokleisli[W[_], A, B](runCokleisli: W[A] => B)

object Cokleisli {
  type Cokleisli$1[W[_]] = { type Type[A, B] = Cokleisli[W, A, B] }

  type Cokleisli$2[W[_], A] = { type Type[B] = Cokleisli[W, A, B] }

  private final class CokleisliTrivialCategoryInstance[W[_]: Comonad] extends Category[Cokleisli$1[W]#Type] {
    override final def catid[A](): Cokleisli[W, A, A] = Cokleisli(Comonad[W].extract)

    override final def catmor[A, B, C](gbc: Cokleisli[W, B, C], gab: Cokleisli[W, A, B]): Cokleisli[W, A, C] = Cokleisli(wa => gbc.runCokleisli(Comonad[W].extend(gab.runCokleisli, wa)))
  }

  implicit def cokleisliTrivialSemigroupoidInstance[W[_]: Comonad]: Semigroupoid[Cokleisli$1[W]#Type] = Category.categoryTrivialSemigroupoidInstance[Cokleisli$1[W]#Type]

  implicit def cokleisliTrivialCategoryInstance[W[_]: Comonad]: Category[Cokleisli$1[W]#Type] = new CokleisliTrivialCategoryInstance[W]()
}
