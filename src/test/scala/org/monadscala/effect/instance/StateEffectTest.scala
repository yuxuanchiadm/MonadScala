package org.monadscala.effect.instance

import org.monadscala.Typelevel._
import org.monadscala.effect.Effects._
import org.monadscala.effect.instance.StateEffect._
import org.scalatest.FunSpec

class StateEffectTest extends FunSpec {
  describe("Test StateEffect") {
    it("Test basic") {
      val prog: TransEff[Id, Int, EList# ::[STATE[Boolean]]# ::|, EList# ::[STATE[Int]]# ::|] = for {
        b <- get()
        _ <- putM(if (b) 1 else 2)
        i <- get()
      } yield i
      assertResult(1)(runPureInit(prog, true :: Init.empty))
    }
  }
}
