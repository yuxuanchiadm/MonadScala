package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.Identity._

class IdentityTest {
  @Test
  def testBasic: Unit = {
    val comonad: Comonad[Identity] = Comonad[Identity]
    import comonad._

    assertEquals(1, extract(Identity(1)))
    assertEquals(extend((fa: Identity[Int]) => fa.runIdentity + 1, Identity(2)), Identity(3))
  }
}
