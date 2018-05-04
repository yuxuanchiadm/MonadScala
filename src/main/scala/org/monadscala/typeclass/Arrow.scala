package org.monadscala.typeclass

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

  implicit final class ***[G[_, _]: Arrow, A, B, C, D](gab: G[A, B]) { def ***(gcd: G[C, D]): G[(A, C), (B, D)] = Arrow[G].splitProduct(gab, gcd) }

  implicit final class &&&[G[_, _]: Arrow, A, B, C](gab: G[A, B]) { def &&&(gac: G[A, C]): G[A, (B, C)] = Arrow[G].fanout(gab, gac) }

  def returnA[G[_, _]: Arrow, A](): G[A, A] = Arrow[G].arr(identity)

  implicit final class ^>>[G[_, _]: Arrow, A, B, C](fab: A => B) { def ^>>(gbc: G[B, C]): G[A, C] = Arrow[G].categoryInstance().catmor(gbc, Arrow[G].arr(fab)) }

  implicit final class >>^[G[_, _]: Arrow, A, B, C](gab: G[A, B]) { def >>^(fbc: B => C): G[A, C] = Arrow[G].categoryInstance().catmor(Arrow[G].arr(fbc), gab) }

  implicit final class <<^[G[_, _]: Arrow, A, B, C](gbc: G[B, C]) { def <<^(fab: A => B): G[A, C] = Arrow[G].categoryInstance().catmor(gbc, Arrow[G].arr(fab)) }

  implicit final class ^<<[G[_, _]: Arrow, A, B, C](fbc: B => C) { def ^<<(gab: G[A, B]): G[A, C] = Arrow[G].categoryInstance().catmor(Arrow[G].arr(fbc), gab) }
}
