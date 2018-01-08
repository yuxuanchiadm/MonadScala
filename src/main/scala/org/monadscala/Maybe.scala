package org.monadscala

import scala.language.implicitConversions;

sealed abstract class Maybe[A];
private sealed case class Just[A](value: A) extends Maybe[A];
private sealed case class Empty[A]() extends Maybe[A];

object Maybe {
  private final class MaybeSingleton extends Monad[Maybe] {
    override final def unit[A](a: A): Maybe[A] = Just(a);

    override final def compose[A, B](ma: Maybe[A], famb: A => Maybe[B]): Maybe[B] = ma match {
      case Just(a) => famb(a);
      case Empty() => Empty();
    };
  }

  implicit def singleton: Monad[Maybe] = new MaybeSingleton();

  implicit def forNotation[A](ma: Maybe[A]) = Monad.forNotation(singleton, ma);

  def just[A](a: A): Maybe[A] = Just(a);

  def empty[A](): Maybe[A] = Empty();
}