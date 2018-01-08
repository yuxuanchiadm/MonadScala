package org.monadscala

import org.junit.Test;
import org.junit.Assert._;

import org.monadscala.Identity._;

class IdentityTest {
  @Test
  def testBasic: Unit = {
    val comonad: Comonad[Identity] = Comonad[Identity];
    import comonad._;

    assertEquals(1, extract(Identity(1)));
    assertEquals(extend(Identity(2), (fa: Identity[Int]) => fa.runIdentity + 1), Identity(3));
  }
}