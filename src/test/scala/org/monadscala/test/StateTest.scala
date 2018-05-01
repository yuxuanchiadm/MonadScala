package org.monadscala.test

import org.monadscala._
import org.monadscala.State._
import org.scalatest.FunSpec

class StateTest extends FunSpec {
  describe("Test state monad") {
    it("Should respect monad laws") {
      case class NatGen(n: Int) {
        def apply: (Int, NatGen) = (n, NatGen(n + 1))
      }
      def nextNat(natGen: NatGen): (Int, NatGen) = natGen.apply

      val monad: Monad[State$1[NatGen]#Type] = Monad[State$1[NatGen]#Type]
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

      assertResult(0)(evalState(state0, NatGen(0)))
      assertResult(1)(evalState(state1, NatGen(0)))
    }
  }
}
