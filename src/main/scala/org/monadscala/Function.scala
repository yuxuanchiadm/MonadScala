package org.monadscala

object Function {
  type Function$1[A] = { type Type[B] = Function[A, B] }

  private final class FunctionTrivialCategoryInstance extends Category[Function] {
    override final def catid[A](): Function[A, A] = identity

    override final def catmor[A, B, C](gbc: Function[B, C], gab: Function[A, B]): Function[A, C] = gbc.compose(gab)
  }

  implicit def functionTrivialSemigroupoidInstance: Semigroupoid[Function] = Category.categoryTrivialSemigroupoidInstance[Function]

  implicit def functionTrivialCategoryInstance: Category[Function] = new FunctionTrivialCategoryInstance()

  def const[T, U](x: T)(y: U): T = scala.Function.const(x)(y)
}
