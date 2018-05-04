package org.monadscala.test

import org.monadscala.Typelevel._
import org.monadscala.instance._
import org.monadscala.instance.Option._
import org.monadscala.instance.Store._
import org.monadscala.typeclass._
import org.scalatest.FunSpec

class StoreTest extends FunSpec {
  describe("Test store comonad") {
    it("Should respect comonad laws") {
      val comonad: Comonad[Curry2[Store]# <[Int]# <|] = Comonad[Curry2[Store]# <[Int]# <|]
      import comonad._

      val theStore: Store[Int, String] = store((i) => i.toString(), 0)
      assertResult("0")(extract(theStore))
      assertResult("S1")(peek(1, extend((wa: Store[Int, String]) => "S" + extract(wa), theStore)))
      assertResult(0)(pos(theStore))
      assertResult("1")(peek(1, theStore))
      assertResult("1")(peeks((i: Int) => i + 1, theStore))
      assertResult("1")(extract(seek(1, theStore)))
      assertResult("1")(extract(seeks((i: Int) => i + 1, theStore)))
      assertResult(some("0"))(experiment((i: Int) => some(i), theStore)(Functor[Option]))
      assertResult(none)(experiment(Function.const(none), theStore)(Functor[Option]))
    }
  }
}
