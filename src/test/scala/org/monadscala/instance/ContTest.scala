package org.monadscala.instance

import org.monadscala.Typelevel._
import org.monadscala.instance._
import org.monadscala.instance.Cont._
import org.monadscala.typeclass._
import org.scalatest.FunSpec

class ContTest extends FunSpec {
  describe("Test cont monad") {
    it("Should respect monad laws") {
      def cont1[R](n: Int): Cont[R, Int] =
        callCC[R, Int, Int](k => k(n * n))

      def cont2[R](a: Int, b: Int): Cont[R, Int] = for {
        c <- pureCont(a + b)
        d <- pureCont(c * 2)
      } yield d

      assertResult(16)(performCont(cont1[Int](4)))
      assertResult(6)(performCont(cont2[Int](1, 2)))
    }
  }
}
