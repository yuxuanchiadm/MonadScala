package org.monadscala

import org.monadscala.Typelevel._

import scala.language.implicitConversions

object Either {
  private final class EitherTrivialMonadInstance[E] extends Monad[Currying[Either, E]#Type] {
    override final def unit[A](a: A): Either[E, A] = Right(a)

    override final def compose[A, B](ma: Either[E, A], famb: A => Either[E, B]): Either[E, B] = ma match {
      case Left(l) => Left(l)
      case Right(r) => famb(r)
    }
  }

  implicit def eitherTrivialFunctorInstance[E]: Functor[Currying[Either, E]#Type] = Monad.monadTrivialFunctorInstance[Currying[Either, E]#Type]

  implicit def eitherTrivialApplicativeInstance[E]: Applicative[Currying[Either, E]#Type] = Monad.monadTrivialApplicativeInstance[Currying[Either, E]#Type]

  implicit def eitherTrivialMonadInstance[E]: Monad[Currying[Either, E]#Type] = new EitherTrivialMonadInstance[E]()

  implicit def eitherForNotation[E, A](ma: Either[E, A]) = Monad.forNotation[Currying[Either, E]#Type, A](ma)
}
