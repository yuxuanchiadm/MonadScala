package org.monadscala

import scala.language.implicitConversions;

sealed case class Identity[A](runIdentity: A);

object Identity {
  private final class IdentityMonad extends Monad[Identity] {
    override final def unit[A](a: A): Identity[A] = Identity(a);

    override final def compose[A, B](ma: Identity[A], famb: A => Identity[B]): Identity[B] = famb(ma.runIdentity);
  }

  implicit def singleton: Monad[Identity] = new IdentityMonad();

  implicit def forNotation[A](ma: Identity[A]) = Monad.forNotation(singleton, ma);
}
