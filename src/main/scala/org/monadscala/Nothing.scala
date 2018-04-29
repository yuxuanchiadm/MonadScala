package org.monadscala

import scala.language.implicitConversions

object Nothing {
  private final class NothingTrivialSemigroupInstance extends Semigroup[Nothing] {
    override final def assoc(a1: Nothing, a2: Nothing): Nothing = a1
  }

  implicit def nothingTrivialMagmaInstance: Magma[Nothing] = Semigroup.semigroupTrivialMagmaInstance[Nothing]

  implicit def nothingTrivialSemigroupInstance: Semigroup[Nothing] = new NothingTrivialSemigroupInstance()
}
