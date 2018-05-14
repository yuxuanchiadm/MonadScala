package org.monadscala.instance

import org.monadscala.instance.List._
import org.monadscala.typeclass._
import org.scalatest.FunSpec

class ListTest extends FunSpec {
  describe("Test list alternative") {
    it("Should respect alternative laws") {
      val monadPlus: Alternative[List] = Alternative[List]
      import monadPlus._

      val list: List[String] = combine(combine(List("a", "b"), List("c", "d")), empty())
      assertResult(4)(list.length)
    }
  }
}
