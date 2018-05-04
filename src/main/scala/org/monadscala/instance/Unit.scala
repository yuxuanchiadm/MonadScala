package org.monadscala.instance

import org.monadscala.typeclass._

import scala.language.implicitConversions

object Unit {
  private final class UnitTrivialGroupInstance extends Group[Unit] {
    override final def identity(): Unit = ()

    override final def append(a1: Unit, a2: Unit): Unit = ()

    override final def invert(a: Unit): Unit = ()
  }

  implicit def unitTrivialMagmaInstance[S]: Magma[Unit] = Group.groupTrivialMagmaInstance[Unit]

  implicit def unitTrivialSemigroupInstance[S]: Semigroup[Unit] = Group.groupTrivialSemigroupInstance[Unit]

  implicit def unitTrivialMonoidInstance[S]: Monoid[Unit] = Group.groupTrivialMonoidInstance[Unit]

  implicit def unitTrivialGroupInstance: Group[Unit] = new UnitTrivialGroupInstance()
}
