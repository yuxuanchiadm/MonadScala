package org.monadscala.effect.instance

import org.monadscala.Typelevel._
import org.monadscala.effect.Effects._
import org.monadscala.effect.instance.TransEffect._
import org.monadscala.instance.Option._
import org.scalatest.FunSpec

class TransEffectTest extends FunSpec {
  describe("Test TransEffect") {
    it("Test basic") {
      val prog: SimpleEff[Option, Int, EList# ::[TRANS[Option]]# ::|] = for {
        _ <- promote(Option.empty[Int])
        i <- pure(1)
      } yield i
      assertResult(Option.empty[Int])(runInit(prog, () :: Init.empty))
    }
  }
}
