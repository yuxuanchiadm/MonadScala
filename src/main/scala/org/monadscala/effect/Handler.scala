package org.monadscala.effect

import scala.language.higherKinds

abstract class Handler[M[_], E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]] {
  def handle[A, B, RES_IN, RES_OUT](resourceIn: RES_IN, effect: E[A, RES_IN, RES_OUT], f: A => RES_OUT => M[B]): M[B]
}
