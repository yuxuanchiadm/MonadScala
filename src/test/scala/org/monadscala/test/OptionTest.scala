package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.Option._

class OptionTest {
  @Test
  def testBasic: Unit = {
    val monad: Monad[Option] = Monad[Option]
    import monad._

    val maybe: Option[Int] = for {
      a <- unit(1)
      b <- unit("Test " + a)
      c <- unit(b.length())
    } yield c
    assertEquals(some(6), maybe)
  }
}
