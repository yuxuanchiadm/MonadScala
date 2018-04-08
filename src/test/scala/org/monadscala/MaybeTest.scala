package org.monadscala

import org.junit.Test
import org.junit.Assert._

import org.monadscala.Maybe._

class MaybeTest {
  @Test
  def testBasic: Unit = {
    val monad: Monad[Maybe] = Monad[Maybe]
    import monad._

    val maybe: Maybe[Int] = for {
      a <- unit(1)
      b <- unit("Test " + a)
      c <- unit(b.length())
    } yield c
    assertEquals(just(6), maybe)
  }
}