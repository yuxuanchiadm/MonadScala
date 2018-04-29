package org.monadscala

import scala.language.implicitConversions

sealed case class Identity[A](runIdentity: A)

object Identity {
  private final class IdentityTrivialMonadInstance extends Monad[Identity] {
    override final def unit[A](a: A): Identity[A] = Identity(a)

    override final def compose[A, B](ma: Identity[A], famb: A => Identity[B]): Identity[B] = famb(ma.runIdentity)
  }
  private final class IdentityTrivialComonadInstance extends Comonad[Identity] {
    override final def extract[A](wa: Identity[A]): A = wa.runIdentity

    override final def extend[A, B](fwab: Identity[A] => B, wa: Identity[A]): Identity[B] = Identity(fwab(wa))
  }

  implicit def identityTrivialFunctorInstance: Functor[Identity] = Monad.monadTrivialFunctorInstance[Identity]

  implicit def identityTrivialApplicativeInstance: Applicative[Identity] = Monad.monadTrivialApplicativeInstance[Identity]

  implicit def identityTrivialMonadInstance: Monad[Identity] = new IdentityTrivialMonadInstance()

  implicit def identityTrivialComonadInstance: Comonad[Identity] = new IdentityTrivialComonadInstance()

  implicit def identityForNotation[A](ma: Identity[A]) = Monad.forNotation[Identity, A](ma)
}
