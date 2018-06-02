package org.monadscala.instance

import org.monadscala.Typelevel._
import org.monadscala.typeclass._

import scala.language.implicitConversions

sealed case class Cont[R, A](runCont: (A => R) => R)

object Cont {
  private final class ContTrivialMonadInstance[R] extends Monad[Curry2[Cont]# <[R]# <|] {
    override final def unit[A](a: A): Cont[R, A] = Cont(k => k(a))

    override final def compose[A, B](ma: Cont[R, A], famb: A => Cont[R, B]): Cont[R, B] = Cont(k => ma.runCont(a => famb(a).runCont(k)))
  }

  implicit def listTrivialFunctorInstance[R]: Functor[Curry2[Cont]# <[R]# <|] = Monad.monadTrivialFunctorInstance[Curry2[Cont]# <[R]# <|]

  implicit def listTrivialApplicativeInstance[R]: Applicative[Curry2[Cont]# <[R]# <|] = Monad.monadTrivialApplicativeInstance[Curry2[Cont]# <[R]# <|]

  implicit def listTrivialMonadInstance[R]: Monad[Curry2[Cont]# <[R]# <|] = new ContTrivialMonadInstance()

  implicit def listForNotation[R, A](ma: Cont[R, A]) = Monad.forNotation[Curry2[Cont]# <[R]# <|, A](ma)

  def pureCont[R, A](a: A): Cont[R, A] = Cont(k => k(a))

  def performCont[A](cont: Cont[A, A]): A = cont.runCont(identity)

  def mapCont[R, A](f: R => R, cont: Cont[R, A]): Cont[R, A] = Cont(k => f(cont.runCont(k)))

  def withCont[R, A, B](f: (B => R) => A => R, cont: Cont[R, A]): Cont[R, B] = Cont(k => cont.runCont(f(k)))

  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] = Cont(k => (f(a => Cont(_ => k(a)))).runCont(k))
}
