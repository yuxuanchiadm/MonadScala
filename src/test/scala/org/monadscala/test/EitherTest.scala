package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.Either._
import org.monadscala.Typelevel._

class EitherTest {
  @Test
  def testBasic: Unit = {
    val monad: Monad[Currying[Either, String]#Type] = Monad[Currying[Either, String]#Type]
    import monad._

    val maybe: Either[String, Int] = for {
      a <- unit(1)
      _ <- Left("Bar")
    } yield a
    assertEquals(Left("Bar"), maybe)
  }
}
