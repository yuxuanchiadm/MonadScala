package org.monadscala

import org.junit.Test;
import org.junit.Assert._;

import org.monadscala.Lists._;

class ListTest {
  @Test
  def testBasic: Unit = {
    val alternative: Alternative[List] = Alternative[List];
    import alternative._;

    val list: List[String] = combine(combine(List("a", "b"), List("c", "d")), empty);
    assertEquals(4, list.length);
  }
}