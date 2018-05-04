package org.monadscala.typeclass

import scala.language.higherKinds

abstract class Groupoid[G[_, _]] {
  def identity[A](): G[A, A]

  def append[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C]

  def invert[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C]
}

object Groupoid {
  private final class GroupoidTrivialSemigroupoidInstance[G[_, _]: Groupoid] extends Semigroupoid[G] {
    def assoc[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C] = Groupoid[G].append(gbc, gab)
  }

  private final class GroupoidTrivialCategoryInstance[G[_, _]: Groupoid] extends Category[G] {
    override def catid[A](): G[A, A] = Groupoid[G].identity()

    override def catmor[A, B, C](gbc: G[B, C], gab: G[A, B]): G[A, C] = Groupoid[G].append(gbc, gab)
  }

  def apply[G[_, _]: Groupoid]: Groupoid[G] = implicitly[Groupoid[G]]

  def groupoidTrivialSemigroupoidInstance[G[_, _]: Groupoid]: Semigroupoid[G] = new GroupoidTrivialSemigroupoidInstance()

  def groupoidTrivialCategoryInstance[G[_, _]: Groupoid]: Category[G] = new GroupoidTrivialCategoryInstance()
}
