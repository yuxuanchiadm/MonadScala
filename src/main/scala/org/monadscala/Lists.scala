package org.monadscala

import scala.language.implicitConversions;

object Lists {
  private final class ListSingleton extends Alternative[List] {
    override final def pure[A](a: A): List[A] = scala.collection.immutable.List(a);

    override final def apply[A, B](ffab: List[A => B], fa: List[A]): List[B] = for {
      fab <- ffab
      a <- fa
    } yield fab(a);

    override final def empty[A]: List[A] = Nil;

    override final def combine[A](a1: List[A], a2: List[A]): List[A] = a1 ++ a2;
  }

  implicit def singleton: Alternative[List] = new ListSingleton();
}