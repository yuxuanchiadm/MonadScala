package org.monadscala

import org.junit.Test;
import org.junit.Assert._;

import org.monadscala.State._;
import org.monadscala._;

class StateTest {
  @Test
  def testBasic: Unit = {
    case class NatGen(n: Int) {
      def apply: (Int, NatGen) = (n, NatGen(n + 1));
    }
    def nextNat(natGen: NatGen): (Int, NatGen) = natGen.apply;

    val monad: Monad[StatePartial[NatGen]#Type] = Monad[StatePartial[NatGen]#Type];
    import monad._;

    val state = for {
      s0 <- get[NatGen]
      (v0, s1) = nextNat(s0)
      _ <- put(s1)
    } yield v0;

    assertEquals(0, evalState(state, NatGen(0)));
  }
}