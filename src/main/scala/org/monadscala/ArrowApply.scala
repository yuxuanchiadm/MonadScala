package org.monadscala

import scala.language.higherKinds

abstract class ArrowApply[G[_, _]: Arrow] {
  def arrowInstance(): Arrow[G] = Arrow[G]

  def app[A, B](): G[(G[A, B], A), B]
}

object ArrowApply {
  def apply[G[_, _]: ArrowApply]: ArrowApply[G] = implicitly[ArrowApply[G]]
}
