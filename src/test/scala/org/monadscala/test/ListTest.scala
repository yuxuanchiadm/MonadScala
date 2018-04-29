package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.List._

class ListTest {
  @Test
  def testBasic: Unit = {
    val monadPlus: MonadPlus[List] = MonadPlus[List]
    import monadPlus._

    val list: List[String] = mplus(mplus(List("a", "b"), List("c", "d")), mzero)
    assertEquals(4, list.length)
  }
}
