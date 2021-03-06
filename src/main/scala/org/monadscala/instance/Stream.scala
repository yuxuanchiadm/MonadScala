package org.monadscala.instance

import org.monadscala.typeclass._

import scala.language.implicitConversions

sealed case class Stream[A](runStream: (A, () => Stream[A]))

object Stream {
  private final class StreamTrivialApplicativeInstance extends Applicative[Stream] {
    override final def apply[A, B](ffab: Stream[A => B], fa: Stream[A]): Stream[B] = Stream(ffab.runStream._1(fa.runStream._1), () => apply(ffab.runStream._2(), fa.runStream._2()))
  }

  private final class StreamTrivialMonadInstance extends Monad[Stream] {
    override final def unit[A](a: A): Stream[A] = Stream((a, () => unit(a)))

    override final def compose[A, B](ma: Stream[A], famb: A => Stream[B]): Stream[B] = Stream(famb(ma.runStream._1).runStream._1, () => compose(ma.runStream._2(), famb))
  }

  private final class StreamTrivialComonadInstance extends Comonad[Stream] {
    override final def extract[A](wa: Stream[A]): A = wa.runStream._1

    override final def extend[A, B](fwab: Stream[A] => B, wa: Stream[A]): Stream[B] = Stream(fwab(wa), () => extend(fwab, wa.runStream._2()))
  }

  implicit def streamTrivialFunctorInstance: Functor[Stream] = Monad.monadTrivialFunctorInstance[Stream]

  implicit def streamTrivialApplicativeInstance: Applicative[Stream] = new StreamTrivialApplicativeInstance()

  implicit def streamTrivialMonadInstance: Monad[Stream] = new StreamTrivialMonadInstance()

  implicit def streamTrivialComonadInstance: Comonad[Stream] = new StreamTrivialComonadInstance()

  implicit def streamForNotation[A](ma: Stream[A]) = Monad.forNotation[Stream, A](ma)

  implicit final class <:>[A](a: A) { def <:>(ma: Stream[A]): Stream[A] = Stream(a, () => ma) }

  def head[A](ma: Stream[A]): A = ma.runStream._1

  def tail[A](ma: Stream[A]): Stream[A] = ma.runStream._2()

  def inits[A](ma: Stream[A]): Stream[List[A]] = Stream(List(), () => map(ma.runStream._1 :: (_: List[A]), inits(ma.runStream._2())))

  def tails[A](ma: Stream[A]): Stream[Stream[A]] = Stream(ma, () => tails(ma.runStream._2()))

  def map[A, B](fab: A => B, ma: Stream[A]): Stream[B] = Stream(fab(ma.runStream._1), () => map(fab, ma.runStream._2()))

  def iterate[A](faa: A => A, a: A): Stream[A] = Stream(a, () => iterate(faa, faa(a)))

  def repeat[A](a: A): Stream[A] = Stream(a, () => repeat(a))

  def take[A](i: Int, ma: Stream[A]): List[A] = if (i == 0) List() else if (i > 0) ma.runStream._1 :: take(i - 1, ma.runStream._2()) else throw new IndexOutOfBoundsException()

  def drop[A](i: Int, ma: Stream[A]): Stream[A] = if (i == 0) ma else if (i > 0) drop(i - 1, ma.runStream._2()) else throw new IndexOutOfBoundsException()
}
