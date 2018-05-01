package org.monadscala

import org.monadscala.Typelevel._

import scala.language.implicitConversions

object Either {
  private final class EitherTrivialMonadInstance[E] extends Monad[Curry2[Either]# <[E]# <|] {
    override final def unit[A](a: A): Either[E, A] = Right(a)

    override final def compose[A, B](ma: Either[E, A], famb: A => Either[E, B]): Either[E, B] = ma match {
      case Left(l) => Left(l)
      case Right(r) => famb(r)
    }
  }

  implicit def eitherTrivialFunctorInstance[E]: Functor[Curry2[Either]# <[E]# <|] = Monad.monadTrivialFunctorInstance[Curry2[Either]# <[E]# <|]

  implicit def eitherTrivialApplicativeInstance[E]: Applicative[Curry2[Either]# <[E]# <|] = Monad.monadTrivialApplicativeInstance[Curry2[Either]# <[E]# <|]

  implicit def eitherTrivialMonadInstance[E]: Monad[Curry2[Either]# <[E]# <|] = new EitherTrivialMonadInstance[E]()

  implicit def eitherForNotation[E, A](ma: Either[E, A]) = Monad.forNotation[Curry2[Either]# <[E]# <|, A](ma)

  def left[A, B](a: A): Either[A, B] = Left(a)

  def right[A, B](b: B): Either[A, B] = Right(b)
}
