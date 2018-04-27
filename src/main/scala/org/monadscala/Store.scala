package org.monadscala

import scala.language.higherKinds
import scala.language.implicitConversions

import org.monadscala.Typelevel._

sealed case class Store[S, A](runStore: (S => A, S))

object Store {
  private final class StoreSingleton0[S] extends Comonad[Currying[Store, S]#Type] {
    override final def extract[A](wa: Store[S, A]): A = wa.runStore match { case (f, s) => f(s) }

    override final def extend[A, B](fawb: Store[S, A] => B, wa: Store[S, A]): Store[S, B] = wa.runStore match {
      case (f, s0) => Store((s1 => fawb(Store((wa.runStore._1, s1))), s0))
    }
  }

  implicit def storeSingleton0[S]: Comonad[Currying[Store, S]#Type] = new StoreSingleton0[S]()

  def store[S, A](f: S => A, s: S): Store[S, A] = Store((f, s))

  def pos[S, A](wa: Store[S, A]): S = wa.runStore._2

  def peek[S, A](s: S, wa: Store[S, A]): A = wa.runStore._1(s)

  def peeks[S, A](fss: S => S, wa: Store[S, A]): A = wa.runStore match { case (f, s) => f(fss(s)) }

  def seek[S, A](s: S, wa: Store[S, A]): Store[S, A] = Store((wa.runStore._1, s))

  def seeks[S, A](fss: S => S, wa: Store[S, A]): Store[S, A] = wa.runStore match { case (f, s) => Store((f, fss(s))) }

  def experiment[F[_], S, A](fsfs: S => F[S], wa: Store[S, A])(implicit constraint0: Functor[F]): F[A] =
    wa.runStore match { case (f, s) => constraint0.fmap(f, fsfs(s)) }
}
