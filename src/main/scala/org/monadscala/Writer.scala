package org.monadscala

import org.monadscala.Typelevel._

import scala.language.implicitConversions

sealed case class Writer[W, A](runWriter: (A, W))

object Writer {
  private final class WriterTrivialMonadInstance[W: Monoid] extends Monad[Currying[Writer, W]#Type] {
    override final def unit[A](a: A): Writer[W, A] = Writer((a, Monoid[W].mempty()))

    override final def compose[A, B](ma: Writer[W, A], famb: A => Writer[W, B]): Writer[W, B] = ma.runWriter match {
      case (a0, w0) => famb(a0).runWriter match { case (a1, w1) => Writer((a1, Monoid[W].mappend(w0, w1))) }
    }
  }

  implicit def writerTrivialFunctorInstance[W: Monoid]: Functor[Currying[Writer, W]#Type] = Monad.monadTrivialFunctorInstance[Currying[Writer, W]#Type]

  implicit def writerTrivialApplicativeInstance[W: Monoid]: Applicative[Currying[Writer, W]#Type] = Monad.monadTrivialApplicativeInstance[Currying[Writer, W]#Type]

  implicit def writerTrivialMonadInstance[W: Monoid]: Monad[Currying[Writer, W]#Type] = new WriterTrivialMonadInstance[W]()

  implicit def writerForNotation[W: Monoid, A](ma: Writer[W, A]) = Monad.forNotation[Currying[Writer, W]#Type, A](ma)

  def writer[W, A](aw: (A, W)): Writer[W, A] = Writer(aw)

  def tell[W](w: W): Writer[W, Unit] = Writer(((), w))

  def listen[W, A](ma: Writer[W, A]): Writer[W, (A, W)] = ma.runWriter match { case (a, w) => Writer(((a, w), w)) }

  def pass[W, A](mafww: Writer[W, (A, W => W)]): Writer[W, A] = mafww.runWriter match { case ((a, fww), w) => Writer((a, fww(w))) }

  def listens[W, A, B](fwb: W => B, ma: Writer[W, A]): Writer[W, (A, B)] = ma.runWriter match { case (a, w) => Writer(((a, fwb(w)), w)) }

  def censor[W, A](fww: W => W, ma: Writer[W, A]): Writer[W, A] = ma.runWriter match { case (a, w) => Writer((a, fww(w))) }

  def execWriter[W, A](ma: Writer[W, A]): W = ma.runWriter._2

  def mapWriter[W, A, B](f: ((A, W)) => (B, W), ma: Writer[W, A]): Writer[W, B] = Writer(f(ma.runWriter))
}
