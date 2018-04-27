package org.monadscala

import scala.language.implicitConversions

object Unit {
  private final class UnitSingleton0 extends Group[Unit] {
    override final def identity(): Unit = ()

    override final def append(a1: Unit, a2: Unit): Unit = ()

    override final def invert(a: Unit): Unit = ()
  }

  implicit def unitSingleton0: Group[Unit] = new UnitSingleton0()
}
