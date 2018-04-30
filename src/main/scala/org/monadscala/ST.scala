package org.monadscala

import scala.language.existentials
import scala.language.implicitConversions

sealed trait StateWorld[S]
sealed case class ST[S, A](f: StateWorld[S] => (StateWorld[S], A))

object ST {
  type ST$1[S] = { type Type[A] = ST[S, A] }

  final class STRef[S, A] private[ST] (private[ST] var a: A)

  private class STWorld[S] extends StateWorld[S]

  private final class STTrivialMonadInstance[S] extends Monad[ST$1[S]#Type] {
    override final def unit[A](a: A): ST[S, A] = ST(s => (s, a))

    override final def compose[A, B](ma: ST[S, A], famb: A => ST[S, B]): ST[S, B] = ST(s0 => {
      ma.f(s0) match { case (s1, a) => famb(a).f(s1) }
    })
  }

  implicit def stTrivialFunctorInstance[S]: Functor[ST$1[S]#Type] = Monad.monadTrivialFunctorInstance[ST$1[S]#Type]

  implicit def stTrivialApplicativeInstance[S]: Applicative[ST$1[S]#Type] = Monad.monadTrivialApplicativeInstance[ST$1[S]#Type]

  implicit def stTrivialMonadInstance[S]: Monad[ST$1[S]#Type] = new STTrivialMonadInstance[S]()

  implicit def stForNotation[S, A](ma: ST[S, A]) = Monad.forNotation[ST$1[S]#Type, A](ma)

  def runST[A]: (ST[S, A] => A) forSome { type S } = (_: ST[_, A]).f(new STWorld())._2

  def newSTRef[S, A](a: A): ST[S, STRef[S, A]] = ST(s => (s, new STRef[S, A](a)))

  def readSTRef[S, A](stRef: STRef[S, A]): ST[S, A] = ST(s => (s, stRef.a))

  def writeSTRef[S, A](stRef: STRef[S, A], a: A): ST[S, Unit] = ST(s => { stRef.a = a; (s, ()) })

  def modifySTRef[S, A](stRef: STRef[S, A], f: A => A): ST[S, Unit] = for {
    a <- readSTRef(stRef)
    _ <- writeSTRef(stRef, f(a))
  } yield ()
}
