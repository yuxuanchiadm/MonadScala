package org.monadscala.effect

abstract class Default[A] {
  def default(): A
}

object Default {
  def apply[A: Default]: Default[A] = implicitly[Default[A]]
  
  implicit def unitDefaultInstance: Default[Unit] = new Default[Unit] {
    def default(): Unit = ()
  }

  implicit def booleanDefaultInstance: Default[Boolean] = new Default[Boolean] {
    def default(): Boolean = false
  }

  implicit def charDefaultInstance: Default[Char] = new Default[Char] {
    def default(): Char = 0
  }

  implicit def byteDefaultInstance: Default[Byte] = new Default[Byte] {
    def default(): Byte = 0
  }

  implicit def shortDefaultInstance: Default[Short] = new Default[Short] {
    def default(): Short = 0
  }

  implicit def intDefaultInstance: Default[Int] = new Default[Int] {
    def default(): Int = 0
  }

  implicit def longDefaultInstance: Default[Long] = new Default[Long] {
    def default(): Long = 0
  }

  implicit def floatDefaultInstance: Default[Float] = new Default[Float] {
    def default(): Float = 0
  }

  implicit def doubleDefaultInstance: Default[Double] = new Default[Double] {
    def default(): Double = 0
  }

  implicit def tupleDefaultInstance[A: Default, B: Default]: Default[(A, B)] = new Default[(A, B)] {
    def default(): (A, B) = (implicitly[Default[A]].default(), implicitly[Default[B]].default())
  }

  implicit def optionDefaultInstance[A]: Default[Option[A]] = new Default[Option[A]] {
    def default(): Option[A] = Option.empty
  }

  implicit def listDefaultInstance[A]: Default[List[A]] = new Default[List[A]] {
    def default(): List[A] = List.empty
  }
}
