package org.monadscala

import scala.language.higherKinds

import org.monadscala.Typelevel._
import org.scalatest.FunSpec

class TypelevelTest extends FunSpec {
  describe("Test type level currying") {
    it("Should compils") {
      def fun[F]: Unit = ()
      def foo[F[_]]: Unit = ()
      def bar[F[_, _]]: Unit = ()

      assertCompiles("fun[Curry2[Either]# <[List[String]]# <[Option[Int]]# <|]")
      assertCompiles("foo[Curry2[Either]# <[List[String]]# <|]")
      assertCompiles("bar[Curry2[Either]# <|]")
    }
  }
}
