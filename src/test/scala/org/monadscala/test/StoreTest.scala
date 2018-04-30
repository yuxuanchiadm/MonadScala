package org.monadscala.test

import org.junit.Test
import org.junit.Assert._

import org.monadscala._
import org.monadscala.Option._
import org.monadscala.Store._

class StoreTest {
  @Test
  def testBasic: Unit = {
    val comonad: Comonad[Store$1[Int]#Type] = Comonad[Store$1[Int]#Type]
    import comonad._

    val theStore: Store[Int, String] = store((i) => i.toString(), 0)
    assertEquals("0", extract(theStore))
    assertEquals("S1", peek(1, extend((wa: Store[Int, String]) => "S" + extract(wa), theStore)))
    assertEquals(0, pos(theStore))
    assertEquals("1", peek(1, theStore))
    assertEquals("1", peeks((i: Int) => i + 1, theStore))
    assertEquals("1", extract(seek(1, theStore)))
    assertEquals("1", extract(seeks((i: Int) => i + 1, theStore)))
    assertEquals(some("0"), experiment((i: Int) => some(i), theStore)(Functor[Option]))
    assertEquals(none, experiment(Function.const(none), theStore)(Functor[Option]))
  }
}
