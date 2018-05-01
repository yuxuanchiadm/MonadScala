package org.monadscala

import org.monadscala.Typelevel._

import scala.language.higherKinds

sealed case class Kleisli[M[_], A, B](runKleisli: A => M[B])

object Kleisli {
  private final class KleisliTrivialCategoryInstance[M[_]: Monad] extends Category[Curry3[Kleisli]# <[M]# <|] {
    override final def catid[A](): Kleisli[M, A, A] = Kleisli(Monad[M].unit)

    override final def catmor[A, B, C](gbc: Kleisli[M, B, C], gab: Kleisli[M, A, B]): Kleisli[M, A, C] = Kleisli(a => Monad[M].compose(gab.runKleisli(a), gbc.runKleisli))
  }

  implicit def kleisliTrivialSemigroupoidInstance[M[_]: Monad]: Semigroupoid[Curry3[Kleisli]# <[M]# <|] = Category.categoryTrivialSemigroupoidInstance[Curry3[Kleisli]# <[M]# <|]

  implicit def kleisliTrivialCategoryInstance[M[_]: Monad]: Category[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialCategoryInstance[M]()
}
