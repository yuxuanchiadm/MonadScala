package org.monadscala

import org.monadscala.Typelevel._

import scala.language.higherKinds

sealed case class Cokleisli[W[_], A, B](runCokleisli: W[A] => B)

object Cokleisli {
  private final class CokleisliTrivialMonadInstance[W[_], A] extends Monad[Curry3[Cokleisli]# <[W]# <[A]# <|] {
    override final def unit[B](b: B): Cokleisli[W, A, B] = Cokleisli(Function.const(b))

    override final def compose[B, C](mb: Cokleisli[W, A, B], fbmc: B => Cokleisli[W, A, C]): Cokleisli[W, A, C] =
      Cokleisli(wa => fbmc(mb.runCokleisli(wa)).runCokleisli(wa))
  }

  private final class CokleisliTrivialCategoryInstance[W[_]: Comonad] extends Category[Curry3[Cokleisli]# <[W]# <|] {
    override final def catid[A](): Cokleisli[W, A, A] = Cokleisli(Comonad[W].extract)

    override final def catmor[A, B, C](gbc: Cokleisli[W, B, C], gab: Cokleisli[W, A, B]): Cokleisli[W, A, C] = Cokleisli(wa => gbc.runCokleisli(Comonad[W].extend(gab.runCokleisli, wa)))
  }

  private final class CokleisliTrivialArrowInstance[W[_]: Comonad] extends Arrow[Curry3[Cokleisli]# <[W]# <|] {
    override final def arr[A, B](fab: A => B): Cokleisli[W, A, B] = Cokleisli(wa => fab(Comonad[W].extract(wa)))

    override final def splitProduct[A, B, C, D](gab: Cokleisli[W, A, B], gcd: Cokleisli[W, C, D]): Cokleisli[W, (A, C), (B, D)] = Cokleisli(wtac => (
      gab.runCokleisli(Comonad[W].extend(Comonad[W].extract(_: W[(A, C)])._1, wtac)),
      gcd.runCokleisli(Comonad[W].extend(Comonad[W].extract(_: W[(A, C)])._2, wtac))))
  }

  private final class CokleisliTrivialArrowChoiceInstance[W[_]: Comonad] extends ArrowChoice[Curry3[Cokleisli]# <[W]# <|] {
    override final def splitSum[A, B, C, D](gab: Cokleisli[W, A, B], gcd: Cokleisli[W, C, D]): Cokleisli[W, Either[A, C], Either[B, D]] =
      arrowInstance().categoryInstance().catmor(
        Cokleisli((wguebd: W[Cokleisli[W, Unit, Either[B, D]]]) =>
          Comonad[W].extract(wguebd).runCokleisli(Comonad[W].extendSecond((), wguebd))),
        arrowInstance().arr(Either.either(
          (a: A) => arrowInstance().categoryInstance().catmor(
            arrowInstance().categoryInstance().catmor(arrowInstance().arr(Either.left[B, D]), gab),
            arrowInstance().arr(Function.const[A, Unit](a))),
          (c: C) => arrowInstance().categoryInstance().catmor(
            arrowInstance().categoryInstance().catmor(arrowInstance().arr(Either.right[B, D]), gcd),
            arrowInstance().arr(Function.const[C, Unit](c))), _)))
  }

  private final class CokleisliTrivialArrowApplyInstance[W[_]: Comonad] extends ArrowApply[Curry3[Cokleisli]# <[W]# <|] {
    override final def app[A, B](): Cokleisli[W, (Cokleisli[W, A, B], A), B] =
      Cokleisli(wtgaba => Comonad[W].extract(wtgaba)._1.runCokleisli(Comonad[W].extend(Comonad[W].extract(_: W[(Cokleisli[W, A, B], A)])._2, wtgaba)))
  }

  implicit def cokleisliTrivialFunctorInstance[W[_], A]: Functor[Curry3[Cokleisli]# <[W]# <[A]# <|] = Monad.monadTrivialFunctorInstance[Curry3[Cokleisli]# <[W]# <[A]# <|]

  implicit def cokleisliTrivialApplicativeInstance[W[_], A]: Applicative[Curry3[Cokleisli]# <[W]# <[A]# <|] = Monad.monadTrivialApplicativeInstance[Curry3[Cokleisli]# <[W]# <[A]# <|]

  implicit def cokleisliTrivialMonadInstance[W[_], A]: Monad[Curry3[Cokleisli]# <[W]# <[A]# <|] = new CokleisliTrivialMonadInstance[W, A]()

  implicit def cokleisliTrivialSemigroupoidInstance[W[_]: Comonad]: Semigroupoid[Curry3[Cokleisli]# <[W]# <|] = Category.categoryTrivialSemigroupoidInstance[Curry3[Cokleisli]# <[W]# <|]

  implicit def cokleisliTrivialCategoryInstance[W[_]: Comonad]: Category[Curry3[Cokleisli]# <[W]# <|] = new CokleisliTrivialCategoryInstance[W]()

  implicit def cokleisliTrivialArrowInstance[W[_]: Comonad]: Arrow[Curry3[Cokleisli]# <[W]# <|] = new CokleisliTrivialArrowInstance[W]()

  implicit def cokleisliTrivialArrowChoiceInstance[W[_]: Comonad]: ArrowChoice[Curry3[Cokleisli]# <[W]# <|] = new CokleisliTrivialArrowChoiceInstance[W]()

  implicit def cokleisliTrivialArrowApplyInstance[W[_]: Comonad]: ArrowApply[Curry3[Cokleisli]# <[W]# <|] = new CokleisliTrivialArrowApplyInstance[W]()
}
