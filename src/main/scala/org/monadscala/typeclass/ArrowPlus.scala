package org.monadscala.typeclass

import scala.language.higherKinds

abstract class ArrowPlus[G[_, _]: ArrowZero] {
  def arrowZeroInstance(): ArrowZero[G] = ArrowZero[G]

  def plusArrow[A, B](gab1: G[A, B], gab2: G[A, B]): G[A, B]
}

object ArrowPlus {
  def apply[G[_, _]: ArrowPlus]: ArrowPlus[G] = implicitly[ArrowPlus[G]]

  implicit final class <+>[G[_, _]: ArrowPlus, A, B](gab1: G[A, B]) { def <+>(gab2: G[A, B]): G[A, B] = ArrowPlus[G].plusArrow(gab1, gab2) }
}
