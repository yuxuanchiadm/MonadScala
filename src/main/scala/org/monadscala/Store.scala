package org.monadscala

import scala.language.higherKinds
import scala.language.implicitConversions

import org.monadscala.Typelevel._

sealed case class Store[S, A](runStore: (S => A, S))

object Store {
  private final class StoreSingleton[S] extends Comonad[Currying[Store, S]#Type] {
    override final def fmap[A, B](fab: A => B, fa: Store[S, A]): Store[S, B] = fa match {
      case Store((f, s)) => Store(fab.compose(f), s)
    }

    override final def extract[A](wa: Store[S, A]): A = wa match {
      case Store((f, s)) => f(s)
    }

    override final def duplicate[A](wa: Store[S, A]): Store[S, Store[S, A]] = wa match {
      case Store((f, s)) => Store((s1) => Store(f, s1), s)
    }
  }

  implicit def singleton[S]: Comonad[Currying[Store, S]#Type] = new StoreSingleton[S]()

  def store[S, A](f: S => A, s: S): Store[S, A] = Store(f, s)

  def pos[S, A](wa: Store[S, A]): S = wa match {
    case Store((_, s)) => s
  }

  def peek[S, A](s: S, wa: Store[S, A]): A = wa match {
    case Store((f, _)) => f(s)
  }

  def peeks[S, A](fss: S => S, wa: Store[S, A]): A = wa match {
    case Store((f, s)) => f(fss(s))
  }

  def seek[S, A](s: S, wa: Store[S, A]): Store[S, A] = wa match {
    case Store((f, _)) => Store(f, s)
  }

  def seeks[S, A](fss: S => S, wa: Store[S, A]): Store[S, A] = wa match {
    case Store((f, s)) => Store(f, fss(s))
  }

  def experiment[F[_], S, A](fsfs: S => F[S], wa: Store[S, A])(implicit constraint0: Functor[F]): F[A] = wa match {
    case Store((f, s)) => constraint0.fmap(f, fsfs(s))
  }
}