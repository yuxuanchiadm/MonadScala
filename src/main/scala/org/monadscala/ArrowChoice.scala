package org.monadscala

import scala.language.higherKinds

abstract class ArrowChoice[G[_, _]: Arrow] {
  def arrowInstance(): Arrow[G] = Arrow[G]

  def left[A, B, C](gab: G[A, B]): G[Either[A, C], Either[B, C]] = splitSum(gab, Arrow[G].categoryInstance().catid())

  def right[A, B, C](gab: G[A, B]): G[Either[C, A], Either[C, B]] = splitSum(Arrow[G].categoryInstance().catid(), gab)

  def splitSum[A, B, C, D](gab: G[A, B], gcd: G[C, D]): G[Either[A, C], Either[B, D]]

  def fanin[A, B, C](gac: G[A, C], gbc: G[B, C]): G[Either[A, B], C] = Arrow[G].categoryInstance().catmor(Arrow[G].arr((ecc: Either[C, C]) => ecc.merge), splitSum(gac, gbc))
}

object ArrowChoice {
  def apply[G[_, _]: ArrowChoice]: ArrowChoice[G] = implicitly[ArrowChoice[G]]

  implicit final class +++[G[_, _]: ArrowChoice, A, B, C, D](gab: G[A, B]) { def +++(gcd: G[C, D]): G[Either[A, C], Either[B, D]] = ArrowChoice[G].splitSum(gab, gcd) }

  implicit final class |||[G[_, _]: ArrowChoice, A, B, C](gac: G[A, C]) { def |||(gbc: G[B, C]): G[Either[A, B], C] = ArrowChoice[G].fanin(gac, gbc) }
}
