package org.monadscala

import scala.language.implicitConversions

sealed case class State[S, A](runState: S => (A, S))

object State {
  type State$1[S] = { type Type[A] = State[S, A] }

  private final class StateTrivialMonadInstance[S] extends Monad[State$1[S]#Type] {
    override final def unit[A](a: A): State[S, A] = State(s => (a, s))

    override final def compose[A, B](ma: State[S, A], famb: A => State[S, B]): State[S, B] = State(s0 => {
      ma.runState(s0) match { case (a, s1) => famb(a).runState(s1) }
    })
  }

  implicit def stateTrivialFunctorInstance[S]: Functor[State$1[S]#Type] = Monad.monadTrivialFunctorInstance[State$1[S]#Type]

  implicit def stateTrivialApplicativeInstance[S]: Applicative[State$1[S]#Type] = Monad.monadTrivialApplicativeInstance[State$1[S]#Type]

  implicit def stateTrivialMonadInstance[S]: Monad[State$1[S]#Type] = new StateTrivialMonadInstance[S]()

  implicit def stateForNotation[S, A](ma: State[S, A]) = Monad.forNotation[State$1[S]#Type, A](ma)

  def put[S](newState: S): State[S, Unit] = State(s => ((), newState))

  def get[S]: State[S, S] = State(s => (s, s))

  def state[S, A](s: S => (A, S)): State[S, A] = State(s)

  def modify[S](f: S => S): State[S, Unit] = State(s => ((), f(s)))

  def gets[S, A](f: S => A): State[S, A] = State(s => (f(s), s))

  def evalState[S, A](state: State[S, A], s: S): A = state.runState(s)._1

  def execState[S, A](state: State[S, A], s: S): S = state.runState(s)._2
}
