package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.Either._

class EitherTest {
  @Test
  def testBasic: Unit = {
    val monad: Monad[Either$1[String]#Type] = Monad[Either$1[String]#Type]
    import monad._

    val maybe: Either[String, Int] = for {
      a <- unit(1)
      _ <- Left("Bar")
    } yield a
    assertEquals(Left("Bar"), maybe)
  }
}
