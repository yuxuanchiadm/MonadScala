package org.monadscala

import org.monadscala.Typelevel._

import scala.language.implicitConversions

object Either {
  private final class EitherSingleton0[E] extends Monad[Currying[Either, E]#Type] {
    override final def unit[A](a: A): Either[E, A] = Right(a)

    override final def compose[A, B](ma: Either[E, A], famb: A => Either[E, B]): Either[E, B] = ma match {
      case Left(l) => Left(l)
      case Right(r) => famb(r)
    }
  }

  implicit def eitherSingleton0[E]: Monad[Currying[Either, E]#Type] = new EitherSingleton0[E]()

  implicit def eitherForNotation[E, A](ma: Either[E, A]) = Monad.forNotation[Currying[Either, E]#Type, A](ma)
}
