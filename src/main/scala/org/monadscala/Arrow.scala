package org.monadscala

import scala.language.higherKinds

abstract class Arrow[G[_, _]: Category] {
  def categoryInstance(): Category[G] = Category[G]

  def arr[A, B](fab: A => B): G[A, B]

  def first[A, B, C](gab: G[A, B]): G[(A, C), (B, C)] = splitProduct(gab, Category[G].catid())

  def second[A, B, C](gab: G[A, B]): G[(C, A), (C, B)] = splitProduct(Category[G].catid(), gab)

  def splitProduct[A, B, C, D](gab: G[A, B], gcd: G[C, D]): G[(A, C), (B, D)]

  def fanout[A, B, C](gab: G[A, B], gac: G[A, C]): G[A, (B, C)] = Category[G].catmor(splitProduct(gab, gac), arr(a => (a, a)))
}

object Arrow {
  def apply[G[_, _]: Arrow]: Arrow[G] = implicitly[Arrow[G]]
}
