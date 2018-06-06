package org.monadscala.effect

import org.monadscala.ForNotation
import org.monadscala.TList
import org.monadscala.TList._
import org.monadscala.Typelevel._
import org.monadscala.effect.Effects._
import org.monadscala.typeclass._

import scala.language.existentials
import scala.language.higherKinds
import scala.language.implicitConversions

object Effects {
  sealed trait EffM[M[_], A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]]

  type SimpleEff[M[_], A, EFF <: TList[ANY_EFFECT]] = EffM[M, A, EFF, EFF]
  type TransEff[M[_], A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]] = EffM[M, A, EFF_IN, EFF_OUT]

  sealed trait EFFECT[RES, +E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]
  type ANY_EFFECT = EFFECT[RES, E] forSome {
    type RES
    type E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]
  }
  type EList = Make[ANY_EFFECT]

  def unreachable: Nothing = sys.error("unreachable")

  sealed trait EffNeq[A_RES_IN, A_RES_OUT, A_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], B_RES_IN, B_RES_OUT, B_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]

  implicit def implicitEffNeq[A_RES_IN, A_RES_OUT, A_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], B_RES_IN, B_RES_OUT, B_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]: EffNeq[A_RES_IN, A_RES_OUT, A_E, B_RES_IN, B_RES_OUT, B_E] = new EffNeq[A_RES_IN, A_RES_OUT, A_E, B_RES_IN, B_RES_OUT, B_E] {}
  implicit def implicitEffNeqAmbiguous1[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]: EffNeq[RES_IN, RES_OUT, E, RES_IN, RES_OUT, E] = unreachable
  implicit def implicitEffNeqAmbiguous2[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]: EffNeq[RES_IN, RES_OUT, E, RES_IN, RES_OUT, E] = unreachable

  sealed trait EffElem[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]]
  case class EffElemHere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](effNotElem: EffNotElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]) extends EffElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], EFF_OUT]]
  case class EffElemThere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], H_RES_IN, H_RES_OUT, H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](effElem: EffElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT], effNeq: EffNeq[RES_IN, RES_OUT, E, H_RES_IN, H_RES_OUT, H_E]) extends EffElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[H_RES_IN, H_E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[H_RES_OUT, H_E], EFF_OUT]]

  implicit def implicitEffElemHere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](implicit effNotElem: EffNotElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]): EffElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], EFF_OUT]] = EffElemHere(effNotElem)
  implicit def implicitEffElemThere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], H_RES_IN, H_RES_OUT, H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](implicit effElem: EffElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT], effNeq: EffNeq[RES_IN, RES_OUT, E, H_RES_IN, H_RES_OUT, H_E]): EffElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[H_RES_IN, H_E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[H_RES_OUT, H_E], EFF_OUT]] = EffElemThere(effElem, effNeq)

  sealed trait EffNotElem[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]]
  case class EffNotElemNil[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]() extends EffNotElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TNil, TList[ANY_EFFECT]#TNil]
  case class EffNotElemCons[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], H_RES_IN, H_RES_OUT, H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](effNotElem: EffNotElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT], effNeq: EffNeq[RES_IN, RES_OUT, E, H_RES_IN, H_RES_OUT, H_E]) extends EffNotElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[H_RES_IN, H_E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[H_RES_OUT, H_E], EFF_OUT]]

  implicit def implicitEffNotElemNil[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]]: EffNotElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TNil, TList[ANY_EFFECT]#TNil] = EffNotElemNil()
  implicit def implicitEffNotElemCons[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], H_RES_IN, H_RES_OUT, H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](implicit effNotElem: EffNotElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT], effNeq: EffNeq[RES_IN, RES_OUT, E, H_RES_IN, H_RES_OUT, H_E]): EffNotElem[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[H_RES_IN, H_E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[H_RES_OUT, H_E], EFF_OUT]] = EffNotElemCons(effNotElem, effNeq)

  sealed trait EffSubList[SUB_EFF_IN <: TList[ANY_EFFECT], SUB_EFF_OUT <: TList[ANY_EFFECT], SUPER_EFF_IN <: TList[ANY_EFFECT], SUPER_EFF_OUT <: TList[ANY_EFFECT]]
  case class EffSubListNil[SUPER_EFF_IN <: TList[ANY_EFFECT], SUPER_EFF_OUT <: TList[ANY_EFFECT]]() extends EffSubList[TList[ANY_EFFECT]#TNil, TList[ANY_EFFECT]#TNil, SUPER_EFF_IN, SUPER_EFF_OUT]
  case class EffSubListIn[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], ORIGIN_EFF_IN <: TList[ANY_EFFECT], ORIGIN_EFF_OUT <: TList[ANY_EFFECT], SUPER_EFF_IN <: TList[ANY_EFFECT], SUPER_EFF_OUT <: TList[ANY_EFFECT]](effElem: EffElem[RES_IN, RES_OUT, E, SUPER_EFF_IN, SUPER_EFF_OUT], effSubList: EffSubList[ORIGIN_EFF_IN, ORIGIN_EFF_OUT, SUPER_EFF_IN, SUPER_EFF_OUT]) extends EffSubList[TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], ORIGIN_EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], ORIGIN_EFF_OUT], SUPER_EFF_IN, SUPER_EFF_OUT]

  implicit def implicitEffSubListNil[SUPER_EFF_IN <: TList[ANY_EFFECT], SUPER_EFF_OUT <: TList[ANY_EFFECT]]: EffSubList[TList[ANY_EFFECT]#TNil, TList[ANY_EFFECT]#TNil, SUPER_EFF_IN, SUPER_EFF_OUT] = EffSubListNil()
  implicit def implicitEffSubListIn[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], ORIGIN_EFF_IN <: TList[ANY_EFFECT], ORIGIN_EFF_OUT <: TList[ANY_EFFECT], SUPER_EFF_IN <: TList[ANY_EFFECT], SUPER_EFF_OUT <: TList[ANY_EFFECT]](implicit effElem: EffElem[RES_IN, RES_OUT, E, SUPER_EFF_IN, SUPER_EFF_OUT], effSubList: EffSubList[ORIGIN_EFF_IN, ORIGIN_EFF_OUT, SUPER_EFF_IN, SUPER_EFF_OUT]): EffSubList[TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], ORIGIN_EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], ORIGIN_EFF_OUT], SUPER_EFF_IN, SUPER_EFF_OUT] = EffSubListIn(effElem, effSubList)

  sealed trait EffCall[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]]
  case class EffCallHere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]]() extends EffCall[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], EFF], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], EFF]]
  case class EffCallThere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], H_RES, H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](effCall: EffCall[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]) extends EffCall[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[H_RES, H_E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[H_RES, H_E], EFF_OUT]]

  implicit def implicitEffCallHere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]]: EffCall[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], EFF], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], EFF]] = EffCallHere()
  implicit def implicitEffCallThere[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], H_RES, H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](implicit effCall: EffCall[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]): EffCall[RES_IN, RES_OUT, E, TList[ANY_EFFECT]#TCons[EFFECT[H_RES, H_E], EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[H_RES, H_E], EFF_OUT]] = EffCallThere(effCall)

  sealed trait EffLift[UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]]
  case class EffLiftNil[UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT]]() extends EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, TList[ANY_EFFECT]#TNil, TList[ANY_EFFECT]#TNil]
  case class EffLiftCons[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], RESULT_EFF_IN <: TList[ANY_EFFECT], RESULT_EFF_OUT <: TList[ANY_EFFECT]](effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, RESULT_EFF_IN, RESULT_EFF_OUT], effElem: EffElem[RES_IN, RES_OUT, E, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT], effNotElem: EffNotElem[RES_IN, RES_OUT, E, RESULT_EFF_IN, RESULT_EFF_OUT]) extends EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], RESULT_EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], RESULT_EFF_OUT]]
  case class EffLiftLifted[RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], RESULT_EFF_IN <: TList[ANY_EFFECT], RESULT_EFF_OUT <: TList[ANY_EFFECT]](effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, RESULT_EFF_IN, RESULT_EFF_OUT], effNotElem: EffNotElem[RES, RES, E, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT]) extends EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], RESULT_EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES, E], RESULT_EFF_OUT]]

  implicit def implicitEffLiftNil[UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT]]: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, TList[ANY_EFFECT]#TNil, TList[ANY_EFFECT]#TNil] = EffLiftNil()
  implicit def implicitEffLiftCons[RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], RESULT_EFF_IN <: TList[ANY_EFFECT], RESULT_EFF_OUT <: TList[ANY_EFFECT]](implicit effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, RESULT_EFF_IN, RESULT_EFF_OUT], effElem: EffElem[RES_IN, RES_OUT, E, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT], effNotElem: EffNotElem[RES_IN, RES_OUT, E, RESULT_EFF_IN, RESULT_EFF_OUT]): EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], RESULT_EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], RESULT_EFF_OUT]] = EffLiftCons(effLift, effElem, effNotElem)
  implicit def implicitEffLiftLifted[RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], RESULT_EFF_IN <: TList[ANY_EFFECT], RESULT_EFF_OUT <: TList[ANY_EFFECT]](implicit effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, RESULT_EFF_IN, RESULT_EFF_OUT], effNotElem: EffNotElem[RES, RES, E, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT]): EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], RESULT_EFF_IN], TList[ANY_EFFECT]#TCons[EFFECT[RES, E], RESULT_EFF_OUT]] = EffLiftLifted(effLift, effNotElem)

  case class EffCompletedLift[UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]](effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT], effSubList: EffSubList[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT])

  implicit def implicitEffCompletedLift[UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]](implicit effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT], effSubList: EffSubList[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT]) = EffCompletedLift(effLift, effSubList)

  sealed trait Env[M[_], EFF <: TList[ANY_EFFECT]]
  case class EnvNil[M[_]]() extends Env[M, TList[ANY_EFFECT]#TNil]
  case class EnvCons[M[_], RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]](handler: Handler[M, E], resource: RES, env: Env[M, EFF]) extends Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]]

  implicit def implicitEnvNil[M[_]]: Env[M, TList[ANY_EFFECT]#TNil] = EnvNil()
  implicit def implicitEnvCons[M[_], RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]](implicit handler: Handler[M, E], defaultRes: Default[RES], env: Env[M, EFF]): Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]] = EnvCons(handler, defaultRes.default(), env)

  def envHead[M[_], RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]](env: Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]]): (Handler[M, E], RES) = env match { case EnvCons(handler, resource, env) => (handler, resource) }
  def envTail[M[_], RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]](env: Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]]): Env[M, EFF] = env match { case EnvCons(handler, resource, env) => env }

  def envUnlabel[M[_], L, RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]](env: Env[M, TList[ANY_EFFECT]#TCons[EFFECT[LRes[L, RES], E], TList[ANY_EFFECT]#TNil]]): Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], TList[ANY_EFFECT]#TNil]] = env match { case EnvCons(handler, LRes(resource), env) => EnvCons(handler, resource, env) }
  def envRelabel[M[_], L, RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]](env: Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], TList[ANY_EFFECT]#TNil]]): Env[M, TList[ANY_EFFECT]#TCons[EFFECT[LRes[L, RES], E], TList[ANY_EFFECT]#TNil]] = env match { case EnvCons(handler, resource, env) => EnvCons(handler, LRes(resource), env) }

  def envElemIn[M[_], RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](env: Env[M, EFF_IN], effElem: EffElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]): (Handler[M, E], RES_IN) = effElem match {
    case EffElemHere(effNotElem) => envHead(env)
    case EffElemThere(effElem, effNeq) => envElemIn(envTail(env), effElem)
  }
  def envElemOut[M[_], RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](env: Env[M, EFF_OUT], effElem: EffElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]): (Handler[M, E], RES_OUT) = effElem match {
    case EffElemHere(effNotElem) => envHead(env)
    case EffElemThere(effElem, effNeq) => envElemIn(envTail(env), effElem)
  }

  type ANY_EFF_ELEM = EffElem[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT] forSome {
    type RES_IN
    type RES_OUT
    type E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]
    type EFF_IN <: TList[ANY_EFFECT]
    type EFF_OUT <: TList[ANY_EFFECT]
  }
  def asUntypedEffElem(effElem: ANY_EFF_ELEM): UntypedEffElem = effElem match {
    case EffElemHere(effNotElem) => UntypedEffElemHere()
    case EffElemThere(effElem, effNeq) => UntypedEffElemThere(asUntypedEffElem(effElem))
  }
  sealed trait UntypedEffElem
  case class UntypedEffElemHere() extends UntypedEffElem
  case class UntypedEffElemThere(untypedEffElem: UntypedEffElem) extends UntypedEffElem

  type ANY_EFF_LIFT = EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT] forSome {
    type UNLIFTED_EFF_IN <: TList[ANY_EFFECT]
    type UNLIFTED_EFF_OUT <: TList[ANY_EFFECT]
    type LIFTED_EFF_IN <: TList[ANY_EFFECT]
    type LIFTED_EFF_OUT <: TList[ANY_EFFECT]
  }
  def asUntypedEffLift(effLift: ANY_EFF_LIFT): UntypedEffLift = effLift match {
    case EffLiftNil() => UntypedEffLiftNil()
    case EffLiftCons(effLift, effElem, effNotElem) => UntypedEffLiftCons(asUntypedEffLift(effLift), asUntypedEffElem(effElem))
    case EffLiftLifted(effLift, effNotElem) => UntypedEffLiftLifted(asUntypedEffLift(effLift))
  }
  sealed trait UntypedEffLift
  case class UntypedEffLiftNil() extends UntypedEffLift
  case class UntypedEffLiftCons(untypedEffLift: UntypedEffLift, untypedEffElem: UntypedEffElem) extends UntypedEffLift
  case class UntypedEffLiftLifted(untypedEffLift: UntypedEffLift) extends UntypedEffLift

  def effSubListSize(effSubList: EffSubList[_, _, _, _]): Int = effSubList match {
    case EffSubListNil() => 0
    case EffSubListIn(effElem, effSubList) => effSubListSize(effSubList) + 1
  }
  def untypedEffElemIndex(untypedEffElem: UntypedEffElem): Int = untypedEffElem match {
    case UntypedEffElemHere() => 0
    case UntypedEffElemThere(untypedEffElem) => untypedEffElemIndex(untypedEffElem) + 1
  }

  def envUnlift[M[_], UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]](
    liftedEnv: Env[M, LIFTED_EFF_IN],
    effCompletedLift: EffCompletedLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT]): Env[M, UNLIFTED_EFF_IN] = {
    object TypeVariable {
      type RES
      type E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]
      type EFF <: TList[ANY_EFFECT]
    }
    import TypeVariable._
    val unliftedEnvArray: Array[(Handler[M, E], RES)] = Array.ofDim(effSubListSize(effCompletedLift.effSubList))
    def unsafeEnvUnlift(
      liftedEnv: Env[M, _],
      untypedEffLift: UntypedEffLift): Unit = untypedEffLift match {
      case UntypedEffLiftNil() => ()
      case UntypedEffLiftCons(untypedEffLift, untypedEffElem) => liftedEnv match {
        case liftedEnv: EnvNil[M] @unchecked => unreachable
        case liftedEnv: EnvCons[M, RES, E, EFF] @unchecked => {
          unliftedEnvArray(untypedEffElemIndex(untypedEffElem)) = (liftedEnv.handler, liftedEnv.resource)
          unsafeEnvUnlift(liftedEnv.env, untypedEffLift)
        }
      }
      case UntypedEffLiftLifted(untypedEffLift) => liftedEnv match {
        case liftedEnv: EnvNil[M] @unchecked => unreachable
        case liftedEnv: EnvCons[M, RES, E, EFF] @unchecked => unsafeEnvUnlift(liftedEnv.env, untypedEffLift)
      }
    }
    unsafeEnvUnlift(liftedEnv, asUntypedEffLift(effCompletedLift.effLift))
    unliftedEnvArray.foldRight(EnvNil().asInstanceOf[Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]]])({ case ((handler, resource), env) => EnvCons(handler.asInstanceOf[Handler[M, E]], resource.asInstanceOf[RES], env.asInstanceOf[Env[M, EFF]]) }).asInstanceOf[Env[M, UNLIFTED_EFF_IN]]
  }
  def envRelift[M[_], UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]](
    liftedEnv: Env[M, LIFTED_EFF_IN],
    unliftedEnv: Env[M, UNLIFTED_EFF_OUT],
    effLift: EffLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT]): Env[M, LIFTED_EFF_OUT] = effLift match {
    case EffLiftNil() => EnvNil()
    case EffLiftCons(effLift, effElem, effNotElem) => {
      val (handler, resource) = envElemOut(unliftedEnv, effElem)
      EnvCons(handler, resource, envRelift(envTail(liftedEnv), unliftedEnv, effLift))
    }
    case EffLiftLifted(effLift, effNotElem) => {
      val (handler, resource) = envHead(liftedEnv)
      EnvCons(handler, resource, envRelift(envTail(liftedEnv), unliftedEnv, effLift))
    }
  }

  sealed trait Init[RES_LIST <: TList[Any]] { def ::[H_RES](resource: H_RES): Init[TList[Any]#TCons[H_RES, RES_LIST]] }
  object Init { def empty: Init[TList[Any]#TNil] = InitNil() }
  case class InitNil() extends Init[TList[Any]#TNil] { def ::[H_RES](resource: H_RES): Init[TList[Any]#TCons[H_RES, TList[Any]#TNil]] = InitCons(resource, this) }
  case class InitCons[RES, RES_LIST <: TList[Any]](resource: RES, init: Init[RES_LIST]) extends Init[TList[Any]#TCons[RES, RES_LIST]] { def ::[H_RES](resource: H_RES): Init[TList[Any]#TCons[H_RES, TList[Any]#TCons[RES, RES_LIST]]] = InitCons(resource, this) }

  def initHead[RES, RES_LIST <: TList[Any]](init: Init[TList[Any]#TCons[RES, RES_LIST]]): RES = init match { case InitCons(resource, init) => resource }
  def initTail[RES, RES_LIST <: TList[Any]](init: Init[TList[Any]#TCons[RES, RES_LIST]]): Init[RES_LIST] = init match { case InitCons(resource, init) => init }

  sealed trait EffInit[M[_], EFF <: TList[ANY_EFFECT], RES_LIST <: TList[Any]]
  case class EffInitNil[M[_]]() extends EffInit[M, TList[ANY_EFFECT]#TNil, TList[Any]#TNil]
  case class EffInitCons[M[_], RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT], RES_LIST <: TList[Any]](handler: Handler[M, E], effInit: EffInit[M, EFF, RES_LIST]) extends EffInit[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF], TList[Any]#TCons[RES, RES_LIST]]

  implicit def implicitEffInitNil[M[_]]: EffInit[M, TList[ANY_EFFECT]#TNil, TList[Any]#TNil] = EffInitNil()
  implicit def implicitEffInitCons[M[_], RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT], RES_LIST <: TList[Any]](implicit handler: Handler[M, E], effInit: EffInit[M, EFF, RES_LIST]): EffInit[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF], TList[Any]#TCons[RES, RES_LIST]] = EffInitCons(handler, effInit)

  def buildEnv[M[_], EFF <: TList[ANY_EFFECT], RES_LIST <: TList[Any]](effInit: EffInit[M, EFF, RES_LIST], init: Init[RES_LIST]): Env[M, EFF] = {
    object TypeVariable {
      type RES
      type E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]
    }
    import TypeVariable._
    effInit match {
      case effInit: EffInitNil[M] @unchecked => EnvNil()
      case effInit: EffInitCons[M, RES, E, EFF, RES_LIST] @unchecked => EnvCons(effInit.handler, initHead(init), buildEnv(effInit.effInit, initTail(init)))
    }
  }

  sealed case class LRes[L, RES](resource: RES)

  implicit def lresDefaultInstance[L, RES: Default]: Default[LRes[L, RES]] = new Default[LRes[L, RES]] {
    def default(): LRes[L, RES] = LRes(implicitly[Default[RES]].default())
  }

  case class Value[M[_], A, EFF <: TList[ANY_EFFECT]](
    value: A)
    extends EffM[M, A, EFF, EFF]
  case class EBind[M[_], A, B, EFF_IN <: TList[ANY_EFFECT], EFF_OUTIN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    effM: EffM[M, A, EFF_IN, EFF_OUTIN],
    f: A => EffM[M, B, EFF_OUTIN, EFF_OUT])
    extends EffM[M, B, EFF_IN, EFF_OUT]
  case class CallP[M[_], A, RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    effCall: EffCall[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT],
    effect: E[A, RES_IN, RES_OUT])
    extends EffM[M, A, EFF_IN, EFF_OUT]
  case class LiftP[M[_], A, UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]](
    effCompletedLift: EffCompletedLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT],
    effM: EffM[M, A, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT])
    extends EffM[M, A, LIFTED_EFF_IN, LIFTED_EFF_OUT]
  case class New[M[_], A, RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]](
    handler: Handler[M, E],
    resource: RES,
    effM: EffM[M, A, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF], TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]])
    extends EffM[M, A, EFF, EFF]
  case class Label[M[_], A, L, RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]](
    effM: EffM[M, A, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], TList[ANY_EFFECT]#TNil], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], TList[ANY_EFFECT]#TNil]])
    extends EffM[M, A, TList[ANY_EFFECT]#TCons[EFFECT[LRes[L, RES_IN], E], TList[ANY_EFFECT]#TNil], TList[ANY_EFFECT]#TCons[EFFECT[LRes[L, RES_OUT], E], TList[ANY_EFFECT]#TNil]]

  def pure[M[_], A, EFF <: TList[ANY_EFFECT]](
    value: A): EffM[M, A, EFF, EFF] = Value(value)

  def compose[M[_], A, B, EFF_IN <: TList[ANY_EFFECT], EFF_OUTIN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    effM: EffM[M, A, EFF_IN, EFF_OUTIN],
    f: A => EffM[M, B, EFF_OUTIN, EFF_OUT]): EffM[M, B, EFF_IN, EFF_OUT] = EBind(effM, f)

  def call[M[_], A, RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    effect: E[A, RES_IN, RES_OUT])(
    implicit
    effCall: EffCall[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT]): EffM[M, A, EFF_IN, EFF_OUT] = CallP(effCall, effect)

  implicit def lift[M[_], A, UNLIFTED_EFF_IN <: TList[ANY_EFFECT], UNLIFTED_EFF_OUT <: TList[ANY_EFFECT], LIFTED_EFF_IN <: TList[ANY_EFFECT], LIFTED_EFF_OUT <: TList[ANY_EFFECT]](
    effM: EffM[M, A, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT])(
    implicit
    effCompletedLift: EffCompletedLift[UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT]): EffM[M, A, LIFTED_EFF_IN, LIFTED_EFF_OUT] = LiftP(effCompletedLift, effM)

  def effect[T <: ANY_EFFECT]: TTerm[T] = TTerm()
  def newEff[M[_], A, RES, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF <: TList[ANY_EFFECT]](
    effect: TTerm[EFFECT[RES, E]],
    resource: RES,
    effM: EffM[M, A, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF], TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF]])(
    implicit
    handler: Handler[M, E]): EffM[M, A, EFF, EFF] = New(handler, resource, effM)

  def lbl[L]: TTerm[L] = TTerm()
  def label[M[_], A, L, RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]](
    lbl: TTerm[L],
    effM: EffM[M, A, TList[ANY_EFFECT]#TCons[EFFECT[RES_IN, E], TList[ANY_EFFECT]#TNil], TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], TList[ANY_EFFECT]#TNil]]): EffM[M, A, TList[ANY_EFFECT]#TCons[EFFECT[LRes[L, RES_IN], E], TList[ANY_EFFECT]#TNil], TList[ANY_EFFECT]#TCons[EFFECT[LRes[L, RES_OUT], E], TList[ANY_EFFECT]#TNil]] = Label(effM)

  def execEff[M[_], A, B, RES_IN, RES_OUT, E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT], EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    env: Env[M, EFF_IN],
    effCall: EffCall[RES_IN, RES_OUT, E, EFF_IN, EFF_OUT],
    effect: E[A, RES_IN, RES_OUT],
    f: A => Env[M, EFF_OUT] => M[B]): M[B] = {
    object TypeVariable {
      type EFF = EFF_IN
      type RES = RES_IN
      type H_RES
      type H_E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]
    }
    import TypeVariable._
    effCall match {
      case effCall: EffCallHere[RES_IN, RES_OUT, E, EFF] @unchecked => env match {
        case env: EnvNil[M] @unchecked => unreachable
        case env: EnvCons[M, RES, E, EFF] @unchecked => env.handler.handle(env.resource, effect, (a: A) => (resourceOut: RES_OUT) => f(a)(EnvCons(env.handler, resourceOut, env)))
      }
      case effCall: EffCallThere[RES_IN, RES_OUT, E, H_RES, H_E, EFF_IN, EFF_OUT] @unchecked => env match {
        case env: EnvNil[M] @unchecked => unreachable
        case env: EnvCons[M, H_RES, H_E, EFF] @unchecked => execEff(env.env, effCall, effect, (a: A) => (newEnv: Env[M, EFF_OUT]) => f(a)(EnvCons(env.handler, env.resource, newEnv)))
      }
    }
  }

  def eff[M[_], A, B, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    env: Env[M, EFF_IN],
    effM: EffM[M, A, EFF_IN, EFF_OUT],
    f: A => Env[M, EFF_OUT] => M[B]): M[B] = {
    object TypeVariable {
      type EFF_OUTIN <: TList[ANY_EFFECT]
      type RES
      type RES_IN
      type RES_OUT
      type E[A, RES_IN, RES_OUT] <: Effect[A, RES_IN, RES_OUT]
      type L
      type LIFTED_EFF_IN = EFF_IN
      type LIFTED_EFF_OUT = EFF_OUT
      type UNLIFTED_EFF_IN = EFF_IN
      type UNLIFTED_EFF_OUT = EFF_OUT
    }
    import TypeVariable._
    effM match {
      case effM: Value[M, A, EFF_IN] @unchecked => f(effM.value)(env)
      case effM: EBind[M, A, B, EFF_IN, EFF_OUTIN, EFF_OUT] @unchecked => eff(env, effM.effM, (a: A) => (env: Env[M, EFF_OUTIN]) => eff(env, (effM.f(a)), f))
      case effM: CallP[M, A, RES_IN, RES_IN, E, EFF_IN, EFF_OUT] @unchecked => execEff(env, effM.effCall, effM.effect, f)
      case effM: LiftP[M, A, UNLIFTED_EFF_IN, UNLIFTED_EFF_OUT, LIFTED_EFF_IN, LIFTED_EFF_OUT] @unchecked => eff(envUnlift(env, effM.effCompletedLift), effM.effM, (a: A) => (oldEnv: Env[M, UNLIFTED_EFF_OUT]) => f(a)(envRelift(env, oldEnv, effM.effCompletedLift.effLift)))
      case effM: New[M, A, RES, E, EFF_IN] @unchecked => eff(EnvCons(effM.handler, effM.resource, env), effM.effM, (a: A) => (env: Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES, E], EFF_OUT]]) => f(a)(envTail(env)))
      case effM: Label[M, A, L, RES_IN, RES_OUT, E] @unchecked => eff(envUnlabel(env), effM.effM, (a: A) => (env: Env[M, TList[ANY_EFFECT]#TCons[EFFECT[RES_OUT, E], TList[ANY_EFFECT]#TNil]]) => f(a)(envRelabel(env)))
    }
  }

  def run[M[_]: Monad, A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    prog: EffM[M, A, EFF_IN, EFF_OUT])(
    implicit
    env: Env[M, EFF_IN]): M[A] = eff(env, prog, (a: A) => (env: Env[M, EFF_OUT]) => Monad[M].unit(a))

  def runPure[A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    prog: EffM[Id, A, EFF_IN, EFF_OUT])(
    implicit
    env: Env[Id, EFF_IN]): Id[A] = eff(env, prog, (a: A) => (env: Env[Id, EFF_OUT]) => a: Id[A])

  def runWith[M[_], A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](
    f: A => M[A],
    prog: EffM[M, A, EFF_IN, EFF_OUT])(
    implicit
    env: Env[M, EFF_IN]): M[A] = eff(env, prog, (a: A) => (env: Env[M, EFF_OUT]) => f(a))

  def runInit[M[_]: Monad, A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT], RES_LIST <: TList[Any]](
    prog: EffM[M, A, EFF_IN, EFF_OUT],
    init: Init[RES_LIST])(
    implicit
    effInit: EffInit[M, EFF_IN, RES_LIST]): M[A] = run(prog)(Monad[M], buildEnv(effInit, init))

  def runPureInit[A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT], RES_LIST <: TList[Any]](
    prog: EffM[Id, A, EFF_IN, EFF_OUT],
    init: Init[RES_LIST])(
    implicit
    effInit: EffInit[Id, EFF_IN, RES_LIST]): Id[A] = runPure(prog)(buildEnv(effInit, init))

  def runWithInit[M[_], A, EFF_IN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT], RES_LIST <: TList[Any]](
    f: A => M[A],
    prog: EffM[M, A, EFF_IN, EFF_OUT],
    init: Init[RES_LIST])(
    implicit
    effInit: EffInit[M, EFF_IN, RES_LIST]): M[A] = runWith(f, prog)(buildEnv(effInit, init))

  private final class EffMForNotation[M[_], A, EFF_IN <: TList[ANY_EFFECT], EFF_OUTIN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](effM: EffM[M, A, EFF_IN, EFF_OUTIN]) extends ForNotation[({ type Type[B] = EffM[M, B, EFF_OUTIN, EFF_OUT] })#Type, ({ type Type[B] = EffM[M, B, EFF_IN, EFF_OUT] })#Type, ({ type Type[B] = EffM[M, B, EFF_IN, EFF_OUTIN] })#Type, A] {
    override def flatMap[B](famb: A => EffM[M, B, EFF_OUTIN, EFF_OUT]): EffM[M, B, EFF_IN, EFF_OUT] = EBind(effM, famb)

    override def map[B](fab: A => B): EffM[M, B, EFF_IN, EFF_OUTIN] = EBind(effM, (a: A) => Value(fab(a)))
  }

  implicit def effMForNotation[M[_], A, EFF_IN <: TList[ANY_EFFECT], EFF_OUTIN <: TList[ANY_EFFECT], EFF_OUT <: TList[ANY_EFFECT]](effM: EffM[M, A, EFF_IN, EFF_OUTIN]): ForNotation[({ type Type[B] = EffM[M, B, EFF_OUTIN, EFF_OUT] })#Type, ({ type Type[B] = EffM[M, B, EFF_IN, EFF_OUT] })#Type, ({ type Type[B] = EffM[M, B, EFF_IN, EFF_OUTIN] })#Type, A] = new EffMForNotation(effM)
}
