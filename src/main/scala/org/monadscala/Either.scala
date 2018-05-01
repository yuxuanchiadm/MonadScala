package org.monadscala

import scala.language.implicitConversions

object Either {
  type Either$1[A] = { type Type[B] = Either[A, B] }

  private final class EitherTrivialMonadInstance[E] extends Monad[Either$1[E]#Type] {
    override final def unit[A](a: A): Either[E, A] = Right(a)

    override final def compose[A, B](ma: Either[E, A], famb: A => Either[E, B]): Either[E, B] = ma match {
      case Left(l) => Left(l)
      case Right(r) => famb(r)
    }
  }

  implicit def eitherTrivialFunctorInstance[E]: Functor[Either$1[E]#Type] = Monad.monadTrivialFunctorInstance[Either$1[E]#Type]

  implicit def eitherTrivialApplicativeInstance[E]: Applicative[Either$1[E]#Type] = Monad.monadTrivialApplicativeInstance[Either$1[E]#Type]

  implicit def eitherTrivialMonadInstance[E]: Monad[Either$1[E]#Type] = new EitherTrivialMonadInstance[E]()

  implicit def eitherForNotation[E, A](ma: Either[E, A]) = Monad.forNotation[Either$1[E]#Type, A](ma)

  def left[A, B](a: A): Either[A, B] = Left(a)

  def right[A, B](b: B): Either[A, B] = Right(b)
}
