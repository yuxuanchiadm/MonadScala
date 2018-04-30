package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.ST._

class STTest {
  @Test
  def testBasic: Unit = {
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

    assertEquals("Bar1", runST(strST))
  }
}
