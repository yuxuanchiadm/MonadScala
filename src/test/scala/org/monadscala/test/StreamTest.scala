package org.monadscala.test

import org.monadscala._
import org.monadscala.Stream._
import org.monadscala.Typelevel._
import org.scalatest.FunSpec

class StreamTest extends FunSpec {
  describe("Test stream comonad") {
    it("Should respect comonad laws") {
      val comonad: Comonad[Stream] = Comonad[Stream]
      import comonad._

      def natStream(i: Int): Stream[Int] = Stream(i, () => natStream(i + 1))
      assertResult(4)(extend((s: Stream[Int]) => extract(s) * 2, natStream(0)).runStream._2().runStream._2().runStream._1)
      assertResult(4)(Comonad.liftW2((a: Int) => (b: Int) => a * b, natStream(0), natStream(2)).runStream._2().runStream._2().runStream._1)
    }
  }
  describe("Test stream monad") {
    it("Should respect monad laws") {
      val monad: Monad[Stream] = Monad[Stream]
      import monad._

      def natStream(i: Int): Stream[Int] = Stream(i, () => natStream(i + 1))
      assertResult(4)(compose(natStream(0), (i: Int) => unit(i * 2)).runStream._2().runStream._2().runStream._1)
      assertResult(4)(Monad.liftM2((a: Int) => (b: Int) => a * b, natStream(0), natStream(2)).runStream._2().runStream._2().runStream._1)
    }
  }
  describe("Test stream applicative") {
    it("Should respect applicative laws") {
      val applicative: Applicative[Stream] = Applicative[Stream]
      import applicative._

      def natStream(i: Int): Stream[Int] = Stream(i, () => natStream(i + 1))
      assertResult(8)(Applicative.liftMA2((a: Int) => (b: Int) => a * b, natStream(0), natStream(2)).runStream._2().runStream._2().runStream._1)
      assertResult(8)(Applicative.liftWA2((a: Int) => (b: Int) => a * b, natStream(0), natStream(2)).runStream._2().runStream._2().runStream._1)
    }
  }
}
