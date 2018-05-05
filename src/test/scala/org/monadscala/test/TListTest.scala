package org.monadscala.test

import scala.language.higherKinds

import org.monadscala.TList._
import org.scalatest.FunSpec

class TListTest extends FunSpec {
  describe("Test TList") {
    it("Should compils") {
      def validType[T]: Unit = ()
      assertCompiles("validType[Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]")
    }
    it("Elem should work") {
    	assertCompiles("implicitly[Elem[Int, Make[Any]# ::[Int]# ::|]]")
    	assertCompiles("implicitly[Elem[Int, Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]]")
      assertDoesNotCompile("implicitly[Elem[Int, Make[Any]# ::|]]")
      assertDoesNotCompile("implicitly[Elem[Double, Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]]")
    }
    it("SubList should work") {
      assertCompiles("implicitly[SubList[Make[Any]# ::|, Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]]")
      assertCompiles("implicitly[SubList[Make[Any]# ::[Int]# ::|, Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]]")
      assertDoesNotCompile("implicitly[SubList[Make[Any]# ::[Double]# ::|, Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]]")
      assertDoesNotCompile("implicitly[SubList[Make[Any]# ::[Int, Double]# ::|, Make[Any]# ::[String]# ::[Char]# ::[Int]# ::[Long]# ::|]]")
    }
  }
}
