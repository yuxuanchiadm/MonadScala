package org.monadscala

import org.monadscala.Typelevel._

import scala.language.higherKinds

sealed case class Cokleisli[W[_], A, B](runCokleisli: W[A] => B)

object Cokleisli {
  private final class CokleisliTrivialCategoryInstance[W[_]: Comonad] extends Category[Curry3[Cokleisli]# <[W]# <|] {
    override final def catid[A](): Cokleisli[W, A, A] = Cokleisli(Comonad[W].extract)

    override final def catmor[A, B, C](gbc: Cokleisli[W, B, C], gab: Cokleisli[W, A, B]): Cokleisli[W, A, C] = Cokleisli(wa => gbc.runCokleisli(Comonad[W].extend(gab.runCokleisli, wa)))
  }

  implicit def cokleisliTrivialSemigroupoidInstance[W[_]: Comonad]: Semigroupoid[Curry3[Cokleisli]# <[W]# <|] = Category.categoryTrivialSemigroupoidInstance[Curry3[Cokleisli]# <[W]# <|]

  implicit def cokleisliTrivialCategoryInstance[W[_]: Comonad]: Category[Curry3[Cokleisli]# <[W]# <|] = new CokleisliTrivialCategoryInstance[W]()
}
