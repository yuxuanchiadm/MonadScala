package org.monadscala.test

import org.monadscala._
import org.monadscala.Monad._
import org.monadscala.Either._
import org.scalatest.FunSpec

class EitherTest extends FunSpec {
  describe("Test either monad") {
    it("Should compiles") {
      assertCompiles("((right[Int, Int](0)) >>= (i => left[Int, Int](1)))")
    }
    it("Should respect monad laws") {
      val monad: Monad[Either$1[String]#Type] = Monad[Either$1[String]#Type]
      import monad._

      val either: Either[String, Int] = for {
        a <- unit(1)
        _ <- left("Bar")
      } yield a

      assertResult(left("Bar"))(either)
    }
  }
}
