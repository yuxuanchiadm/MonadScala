package org.monadscala

import org.junit.Test;
import org.junit.Assert._;

import org.monadscala.Store._;

class StoreTest {
  @Test
  def testBasic: Unit = {
    val comonad: Comonad[StorePartial[Int]#Type] = Comonad[StorePartial[Int]#Type];
    import comonad._;

    val theStore: Store[Int, String] = store((i) => i.toString(), 0);
    assertEquals("0", extract(theStore));
    assertEquals(0, pos(theStore));
    assertEquals("1", peek(1, theStore));
    assertEquals("1", peeks((i: Int) => i + 1, theStore));
    assertEquals("1", extract(seek(1, theStore)));
    assertEquals("1", extract(seeks((i: Int) => i + 1, theStore)));
    assertEquals(Maybe.just("0"), experiment((i: Int) => Maybe.just(i), theStore)(Monad[Maybe]));
    assertEquals(Maybe.empty(), experiment(Function.const(Maybe.empty[Int]()), theStore)(Monad[Maybe]));
  }
}