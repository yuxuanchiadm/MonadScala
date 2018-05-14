package org.monadscala.instance

import org.monadscala.instance.Option._
import org.monadscala.typeclass._
import org.scalatest.FunSpec

class OptionTest extends FunSpec {
  describe("Test option monad") {
    it("Should respect monad laws") {
      val monad: Monad[Option] = Monad[Option]
      import monad._

      val maybe: Option[Int] = for {
        a <- unit(1)
        b <- unit("Test " + a)
        c <- unit(b.length())
      } yield c
      assertResult(some(6))(maybe)
    }
  }
}
