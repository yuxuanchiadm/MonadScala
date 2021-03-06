package org.monadscala.instance

import org.monadscala.instance._
import org.monadscala.instance.Identity._
import org.monadscala.typeclass._
import org.scalatest.FunSpec

class IdentityTest extends FunSpec {
  describe("Test identity comonad") {
    it("Should respect comonad laws") {
      val comonad: Comonad[Identity] = Comonad[Identity]
      import comonad._

      assertResult(1)(extract(Identity(1)))
      assertResult(Identity(3))(extend((fa: Identity[Int]) => fa.runIdentity + 1, Identity(2)))
    }
  }
}
