package org.monadscala

import org.monadscala.Typelevel._

import scala.language.implicitConversions

sealed case class Reader[R, A](runReader: R => A)

object Reader {
  private final class ReaderSingleton0[R] extends Monad[Currying[Reader, R]#Type] {
    override final def unit[A](a: A): Reader[R, A] = Reader(r => a)

    override final def compose[A, B](ma: Reader[R, A], famb: A => Reader[R, B]): Reader[R, B] = Reader(r => (famb(ma.runReader(r))).runReader(r))
  }

  implicit def ReaderSingleton0[R]: Monad[Currying[Reader, R]#Type] = new ReaderSingleton0[R]()

  implicit def ReaderForNotation[R, A](ma: Reader[R, A]) = Monad.forNotation[Currying[Reader, R]#Type, A](ma)

  def ask[R](): Reader[R, R] = Reader(r => r)

  def local[R, A](frr: R => R, ma: Reader[R, A]): Reader[R, A] = Reader(r => ma.runReader(frr(r)))

  def reader[R, A](fra: R => A): Reader[R, A] = Reader(fra)

  def asks[R, A](fra: R => A): Reader[R, A] = Reader(fra)

  def mapReader[R, A, B](fab: A => B, ma: Reader[R, A]): Reader[R, B] = Reader(r => fab(ma.runReader(r)))

  def withReader[R, S, A](fsr: S => R, ma: Reader[R, A]): Reader[S, A] = Reader(s => ma.runReader(fsr(s)))
}
