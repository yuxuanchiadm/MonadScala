package org.monadscala.instance

import org.monadscala.typeclass._
import org.monadscala.Typelevel._

import scala.language.implicitConversions

sealed case class State[S, A](runState: S => (A, S))

object State {
  private final class StateTrivialMonadInstance[S] extends Monad[Curry2[State]# <[S]# <|] {
    override final def unit[A](a: A): State[S, A] = State(s => (a, s))

    override final def compose[A, B](ma: State[S, A], famb: A => State[S, B]): State[S, B] = State(s0 => {
      ma.runState(s0) match { case (a, s1) => famb(a).runState(s1) }
    })
  }

  implicit def stateTrivialFunctorInstance[S]: Functor[Curry2[State]# <[S]# <|] = Monad.monadTrivialFunctorInstance[Curry2[State]# <[S]# <|]

  implicit def stateTrivialApplicativeInstance[S]: Applicative[Curry2[State]# <[S]# <|] = Monad.monadTrivialApplicativeInstance[Curry2[State]# <[S]# <|]

  implicit def stateTrivialMonadInstance[S]: Monad[Curry2[State]# <[S]# <|] = new StateTrivialMonadInstance[S]()

  implicit def stateForNotation[S, A](ma: State[S, A]) = Monad.forNotation[Curry2[State]# <[S]# <|, A](ma)

  def put[S](newState: S): State[S, Unit] = State(s => ((), newState))

  def get[S]: State[S, S] = State(s => (s, s))

  def state[S, A](s: S => (A, S)): State[S, A] = State(s)

  def modify[S](f: S => S): State[S, Unit] = State(s => ((), f(s)))

  def gets[S, A](f: S => A): State[S, A] = State(s => (f(s), s))

  def evalState[S, A](state: State[S, A], s: S): A = state.runState(s)._1

  def execState[S, A](state: State[S, A], s: S): S = state.runState(s)._2
}
