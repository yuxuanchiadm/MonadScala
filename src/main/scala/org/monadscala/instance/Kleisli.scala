package org.monadscala.instance

import org.monadscala.typeclass._

import org.monadscala.Typelevel._

import scala.language.higherKinds

sealed case class Kleisli[M[_], A, B](runKleisli: A => M[B])

object Kleisli {
  private final class KleisliTrivialCategoryInstance[M[_]: Monad] extends Category[Curry3[Kleisli]# <[M]# <|] {
    override final def catid[A](): Kleisli[M, A, A] = Kleisli(Monad[M].unit)

    override final def catmor[A, B, C](gbc: Kleisli[M, B, C], gab: Kleisli[M, A, B]): Kleisli[M, A, C] =
      Kleisli(a => Monad[M].compose(gab.runKleisli(a), gbc.runKleisli))
  }

  private final class KleisliTrivialArrowInstance[M[_]: Monad] extends Arrow[Curry3[Kleisli]# <[M]# <|] {
    override final def arr[A, B](fab: A => B): Kleisli[M, A, B] = Kleisli(fab.andThen(Monad[M].unit))

    override final def splitProduct[A, B, C, D](gab: Kleisli[M, A, B], gcd: Kleisli[M, C, D]): Kleisli[M, (A, C), (B, D)] =
      Kleisli({ case (a, c) => Monad[M].compose(gab.runKleisli(a), (b: B) => Monad[M].compose(gcd.runKleisli(c), (d: D) => Monad[M].unit((b, d)))) })
  }

  private final class KleisliTrivialArrowZeroInstance[M[_]: Alternative: Monad] extends ArrowZero[Curry3[Kleisli]# <[M]# <|]{
    override final def zeroArrow[A, B](): Kleisli[M, A, B] = Kleisli(a => Alternative[M].empty())
  }

  private final class KleisliTrivialArrowPlusInstance[M[_]: Alternative: Monad] extends ArrowPlus[Curry3[Kleisli]# <[M]# <|] {
    override final def plusArrow[A, B](gab1: Kleisli[M, A, B], gab2: Kleisli[M, A, B]): Kleisli[M, A, B] =
      Kleisli(a => Alternative[M].combine(gab1.runKleisli(a), gab2.runKleisli(a)))
  }

  private final class KleisliTrivialArrowChoiceInstance[M[_]: Monad] extends ArrowChoice[Curry3[Kleisli]# <[M]# <|] {
    override final def splitSum[A, B, C, D](gab: Kleisli[M, A, B], gcd: Kleisli[M, C, D]): Kleisli[M, Either[A, C], Either[B, D]] = Kleisli(Either.either(
      (arrowInstance().categoryInstance().catmor(arrowInstance().arr(Either.left[B, D]), gab)).runKleisli,
      (arrowInstance().categoryInstance().catmor(arrowInstance().arr(Either.right[B, D]), gcd)).runKleisli, _))
  }

  private final class KleisliTrivialArrowApplyInstance[M[_]: Monad] extends ArrowApply[Curry3[Kleisli]# <[M]# <|] {
    override final def app[A, B](): Kleisli[M, (Kleisli[M, A, B], A), B] = Kleisli({ case (mab, a) => mab.runKleisli(a) })
  }

  implicit def kleisliTrivialSemigroupoidInstance[M[_]: Monad]: Semigroupoid[Curry3[Kleisli]# <[M]# <|] = Category.categoryTrivialSemigroupoidInstance[Curry3[Kleisli]# <[M]# <|]

  implicit def kleisliTrivialCategoryInstance[M[_]: Monad]: Category[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialCategoryInstance[M]()

  implicit def kleisliTrivialArrowInstance[M[_]: Monad]: Arrow[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialArrowInstance[M]()

  implicit def kleisliTrivialArrowZeroInstance[M[_]: Alternative: Monad]: ArrowZero[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialArrowZeroInstance[M]()

  implicit def kleisliTrivialArrowPlusInstance[M[_]: Alternative: Monad]: ArrowPlus[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialArrowPlusInstance[M]()

  implicit def kleisliTrivialArrowChoiceInstance[M[_]: Monad]: ArrowChoice[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialArrowChoiceInstance[M]()

  implicit def kleisliTrivialArrowApplyInstance[M[_]: Monad]: ArrowApply[Curry3[Kleisli]# <[M]# <|] = new KleisliTrivialArrowApplyInstance[M]()
}
