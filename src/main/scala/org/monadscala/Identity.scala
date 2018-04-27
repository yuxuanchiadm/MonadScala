package org.monadscala

import scala.language.implicitConversions

sealed case class Identity[A](runIdentity: A)

object Identity {
  private final class IdentitySingleton0 extends Monad[Identity] with Comonad[Identity] {
    override final def unit[A](a: A): Identity[A] = Identity(a)

    override final def compose[A, B](ma: Identity[A], famb: A => Identity[B]): Identity[B] = famb(ma.runIdentity)

    override final def extract[A](wa: Identity[A]): A = wa.runIdentity

    override final def extend[A, B](fawb: Identity[A] => B, wa: Identity[A]): Identity[B] = Identity(fawb(wa))
  }

  implicit def identitySingleton0: Monad[Identity] with Comonad[Identity] = new IdentitySingleton0()

  implicit def identityForNotation[A](ma: Identity[A]) = Monad.forNotation[Identity, A](ma)
}
