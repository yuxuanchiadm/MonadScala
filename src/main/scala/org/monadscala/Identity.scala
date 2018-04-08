package org.monadscala

import scala.language.implicitConversions

sealed case class Identity[A](runIdentity: A)

object Identity {
  private final class IdentitySingleton extends Monad[Identity] with Comonad[Identity] {
    override final def unit[A](a: A): Identity[A] = Identity(a)

    override final def compose[A, B](ma: Identity[A], famb: A => Identity[B]): Identity[B] = famb(ma.runIdentity)

    override final def extract[A](wa: Identity[A]): A = wa.runIdentity

    override final def duplicate[A](wa: Identity[A]): Identity[Identity[A]] = Identity(wa)
  }

  implicit def singleton: Monad[Identity] with Comonad[Identity] = new IdentitySingleton()

  implicit def forNotation[A](ma: Identity[A]) = Monad.forNotation(singleton, ma)
}
