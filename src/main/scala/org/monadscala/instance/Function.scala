package org.monadscala.instance

import org.monadscala.typeclass._

object Function {
  private final class FunctionTrivialCategoryInstance extends Category[Function] {
    override final def catid[A](): Function[A, A] = identity

    override final def catmor[A, B, C](gbc: Function[B, C], gab: Function[A, B]): Function[A, C] = gbc.compose(gab)
  }

  private final class FunctionTrivialArrowInstance extends Arrow[Function] {
    override final def arr[A, B](fab: A => B): Function[A, B] = fab

    override final def splitProduct[A, B, C, D](gab: Function[A, B], gcd: Function[C, D]): Function[(A, C), (B, D)] = { case (a, c) => (gab(a), gcd(c)) }
  }

  private final class FunctionTrivialArrowChoiceInstance extends ArrowChoice[Function] {
    override final def splitSum[A, B, C, D](gab: Function[A, B], gcd: Function[C, D]): Function[Either[A, C], Either[B, D]] = eac => eac.left.map(gab).right.map(gcd)
  }

  private final class FunctionTrivialArrowApplyInstance extends ArrowApply[Function] {
    override final def app[A, B](): Function[(Function[A, B], A), B] = { case (f, a) => f(a) }
  }

  implicit def functionTrivialSemigroupoidInstance: Semigroupoid[Function] = Category.categoryTrivialSemigroupoidInstance[Function]

  implicit def functionTrivialCategoryInstance: Category[Function] = new FunctionTrivialCategoryInstance()

  implicit def functionTrivialArrowInstance: Arrow[Function] = new FunctionTrivialArrowInstance()

  implicit def functionTrivialArrowChoiceInstance: ArrowChoice[Function] = new FunctionTrivialArrowChoiceInstance()

  implicit def functionTrivialArrowApplyInstance: ArrowApply[Function] = new FunctionTrivialArrowApplyInstance()

  def const[T, U](x: T)(y: U): T = scala.Function.const(x)(y)
}
