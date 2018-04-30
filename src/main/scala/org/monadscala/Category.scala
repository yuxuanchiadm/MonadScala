package org.monadscala

import scala.language.higherKinds

abstract class Category[G[_, _]] {
  def catid[A](): G[A, A]

  def catmor[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C]
}

object Category {
  private final class CategoryTrivialSemigroupoidInstance[G[_, _]: Category] extends Semigroupoid[G] {
    def assoc[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C] = Category[G].catmor(gbc, gab)
  }

  def apply[G[_, _]: Category]: Category[G] = implicitly[Category[G]]

  def categoryTrivialSemigroupoidInstance[G[_, _]: Category]: Semigroupoid[G] = new CategoryTrivialSemigroupoidInstance()
}
