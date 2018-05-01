package org.monadscala.test

import org.monadscala._
import org.monadscala.List._
import org.scalatest.FunSpec

class ListTest extends FunSpec {
  describe("Test list monad plus") {
    it("Should respect monad plus laws") {
      val monadPlus: MonadPlus[List] = MonadPlus[List]
      import monadPlus._

      val list: List[String] = mplus(mplus(List("a", "b"), List("c", "d")), mzero)
      assertResult(4)(list.length)
    }
  }
}
