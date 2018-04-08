package org.monadscala

import org.monadscala.Typelevel._;

import scala.language.existentials;
import scala.language.implicitConversions;

sealed trait StateWorld[S]
sealed case class ST[S, A](f: StateWorld[S] => (StateWorld[S], A))

object ST {
  final class STRef[S, A] private[ST] (private[ST] var a: A)

  private class STWorld[S] extends StateWorld[S]

  private final class STMonad[S] extends Monad[Currying[ST, S]#Type] {
    override final def unit[A](a: A): ST[S, A] = ST(s => (s, a));

    override final def compose[A, B](ma: ST[S, A], famb: A => ST[S, B]): ST[S, B] = ST(s0 => {
      val (s1, a) = ma.f(s0);
      famb(a).f(s1);
    });
  }

  implicit def singleton[S]: Monad[Currying[ST, S]#Type] = new STMonad[S]();

  implicit def forNotation[S, A](ma: ST[S, A]) = Monad.forNotation[Currying[ST, S]#Type, A](singleton, ma);

  def runST[A]: (ST[S, A] => A) forSome { type S } = (_: ST[_, A]).f(new STWorld())._2

  def newSTRef[S, A](a: A): ST[S, STRef[S, A]] = ST(s => (s, new STRef[S, A](a)))

  def readSTRef[S, A](stRef: STRef[S, A]): ST[S, A] = ST(s => (s, stRef.a))

  def writeSTRef[S, A](stRef: STRef[S, A], a: A): ST[S, Unit] = ST(s => { stRef.a = a; (s, ()) })

  def modifySTRef[S, A](stRef: STRef[S, A], f: A => A): ST[S, Unit] = for {
    a <- readSTRef(stRef)
    _ <- writeSTRef(stRef, f(a))
  } yield ()
}