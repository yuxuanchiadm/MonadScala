package org.monadscala.effect.instance

import org.monadscala.effect._
import org.monadscala.effect.Effects._

import scala.language.higherKinds

sealed trait StateEffect[A, RES_IN, RES_OUT] extends Effect[A, RES_IN, RES_OUT]
case class StateEffectGet[A]() extends StateEffect[A, A, A]
case class StateEffectPut[RES_IN, RES_OUT](resourceOut: RES_OUT) extends StateEffect[Unit, RES_IN, RES_OUT]

object StateEffect {
  implicit def stateEffectHandlerInstance[M[_]]: Handler[M, StateEffect] = new Handler[M, StateEffect] {
    def handle[A, B, RES_IN, RES_OUT](resourceIn: RES_IN, effect: StateEffect[A, RES_IN, RES_OUT], f: A => RES_OUT => M[B]): M[B] = effect match {
      case effect: StateEffectGet[A] => f(resourceIn)(resourceIn)
      case effect: StateEffectPut[RES_IN, RES_OUT] => f(())(effect.resourceOut)
    }
  }

  type STATE[A] = EFFECT[A, StateEffect]

  def get[M[_], A](): SimpleEff[M, A, EList# ::[STATE[A]]# ::|] =
    call(StateEffectGet(): StateEffect[A, A, A])

  def put[M[_], A](a: A): SimpleEff[M, Unit, EList# ::[STATE[A]]# ::|] =
    call(StateEffectPut(a): StateEffect[Unit, A, A])

  def putM[M[_], A, B](b: B): TransEff[M, Unit, EList# ::[STATE[A]]# ::|, EList# ::[STATE[B]]# ::|] =
    call(StateEffectPut(b): StateEffect[Unit, A, B])

  def update[M[_], A](f: A => A): SimpleEff[M, Unit, EList# ::[STATE[A]]# ::|] =
    compose(get(), (a: A) => put(f(a)))

  def updateM[M[_], A, B](f: A => B): TransEff[M, Unit, EList# ::[STATE[A]]# ::|, EList# ::[STATE[B]]# ::|] =
    compose(get(), (a: A) => putM[M, A, B](f(a)))

  def locally[M[_], R, A, B](a: A, prog: SimpleEff[M, R, EList# ::[STATE[A]]# ::|]): SimpleEff[M, R, EList# ::[STATE[B]]# ::|] = for {
    b <- get()
    _ <- putM(a)
    r <- prog
    _ <- putM(b)
  } yield r
}
