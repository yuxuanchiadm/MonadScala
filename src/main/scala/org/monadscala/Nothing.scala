package org.monadscala

import scala.language.implicitConversions

object Nothing {
  private final class NothingSingleton0 extends Semigroup[Nothing] {
    override final def assoc(a1: Nothing, a2: Nothing): Nothing = a1
  }

  implicit def nothingSingleton0: Semigroup[Nothing] = new NothingSingleton0()
}
