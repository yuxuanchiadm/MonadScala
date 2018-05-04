package org.monadscala.test

import org.monadscala.instance._
import org.monadscala.instance.ST._
import org.scalatest.FunSpec

class STTest extends FunSpec {
  describe("Test st monad") {
    it("Should respect monad laws") {
      def strST[S]: ST[S, String] = {
        for {
          refStr <- newSTRef("")
          iRef <- newSTRef(0)
          _ <- modifySTRef(iRef, (_: Int) + 1)
          i <- readSTRef(iRef)
          _ <- writeSTRef(refStr, "Bar" + i)
          str <- readSTRef(refStr)
        } yield str
      }

      assertResult("Bar1")(runST(strST))
    }
  }
}
