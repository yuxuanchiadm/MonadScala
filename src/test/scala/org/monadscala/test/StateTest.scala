package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.State._
import org.monadscala.Typelevel._

class StateTest {
  @Test
  def testBasic: Unit = {
    case class NatGen(n: Int) {
      def apply: (Int, NatGen) = (n, NatGen(n + 1))
    }
    def nextNat(natGen: NatGen): (Int, NatGen) = natGen.apply

    val monad: Monad[Currying[State, NatGen]#Type] = Monad[Currying[State, NatGen]#Type]
    import monad._

    val state0: State[NatGen, Int] = for {
      s0 <- get[NatGen]
      (v0, s1) = nextNat(s0)
      _ <- put(s1)
    } yield v0

    val state1: State[NatGen, Int] = for {
      v0 <- state0
      v1 <- state(nextNat)
    } yield v1

    assertEquals(0, evalState(state0, NatGen(0)))
    assertEquals(1, evalState(state1, NatGen(0)))
  }
}
