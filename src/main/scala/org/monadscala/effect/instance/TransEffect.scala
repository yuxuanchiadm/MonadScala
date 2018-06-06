package org.monadscala.effect.instance

import org.monadscala.Typelevel._
import org.monadscala.effect._
import org.monadscala.effect.Effects._
import org.monadscala.typeclass._

import scala.language.higherKinds

sealed trait TransEffect[M[_], A, RES_IN, RES_OUT] extends Effect[A, RES_IN, RES_OUT]
case class TransEffectPromote[M[_], A](ma: M[A]) extends TransEffect[M, A, Unit, Unit]

object TransEffect {
  implicit def transEffectHandlerInstance[M[_]: Monad]: Handler[M, Curry4[TransEffect]# <[M]# <|] = new Handler[M, Curry4[TransEffect]# <[M]# <|] {
    def handle[A, B, RES_IN, RES_OUT](resourceIn: RES_IN, effect: TransEffect[M, A, RES_IN, RES_OUT], f: A => RES_OUT => M[B]): M[B] = effect match {
      case effect: TransEffectPromote[M, A] => Monad[M].compose(effect.ma, (a: A) => f(a)(()))
    }
  }

  type TRANS[M[_]] = EFFECT[Unit, Curry4[TransEffect]# <[M]# <|]

  def promote[M[_], A](ma: M[A]): SimpleEff[M, A, EList# ::[TRANS[M]]# ::|] =
    call(TransEffectPromote(ma))
}
