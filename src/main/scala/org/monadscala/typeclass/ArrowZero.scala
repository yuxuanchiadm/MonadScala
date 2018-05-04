package org.monadscala.typeclass

import scala.language.higherKinds

abstract class ArrowZero[G[_, _]: Arrow] {
  def arrowInstance(): Arrow[G] = Arrow[G]

  def zeroArrow[A, B](): G[A, B]
}

object ArrowZero {
  def apply[G[_, _]: ArrowZero]: ArrowZero[G] = implicitly[ArrowZero[G]]
}
