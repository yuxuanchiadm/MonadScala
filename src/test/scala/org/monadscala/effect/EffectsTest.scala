package org.monadscala.effect

import org.monadscala.Typelevel._
import org.monadscala.effect.Effects._
import org.monadscala.effect.TestEffect._
import org.scalatest.FunSpec

import scala.language.higherKinds

class EffectsTest extends FunSpec {
  describe("Test effect list") {
    it("Should correctly type check") {
      trait T_A
      trait T_B
      trait T_C

      assertCompiles("implicitly[EffCompletedLift[EList# ::|, EList# ::|, EList# ::|, EList# ::|]]")
      assertCompiles("implicitly[EffCompletedLift[EList# ::|, EList# ::|, EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_A]]# ::|]]")
      assertDoesNotCompile("implicitly[EffCompletedLift[EList# ::|, EList# ::|, EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::|]]")
      assertCompiles("implicitly[EffCompletedLift[EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::|, EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::|]]")
      assertCompiles("implicitly[EffCompletedLift[EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::|, EList# ::[TEST[T_A]]# ::[TEST[T_C]]# ::|, EList# ::[TEST[T_B]]# ::[TEST[T_C]]# ::|]]")
      assertDoesNotCompile("implicitly[EffCompletedLift[EList# ::[TEST[T_A]]# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::[TEST[T_B]]# ::|, EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::|]]")
      assertDoesNotCompile("implicitly[EffCompletedLift[EList# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::|, EList# ::[TEST[T_A]]# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::[TEST[T_B]]# ::|]]")
      assertDoesNotCompile("implicitly[EffCompletedLift[EList# ::[TEST[T_A]]# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::[TEST[T_B]]# ::|, EList# ::[TEST[T_A]]# ::[TEST[T_A]]# ::|, EList# ::[TEST[T_B]]# ::[TEST[T_B]]# ::|]]")
    }
  }
  describe("Test effect evaluate") {
    it("Should Value correctly evaluate") {
      val prog: SimpleEff[Id, Int, EList# ::|] = for {
        a <- pure(1)
      } yield a
      assertResult(1)(runPure(prog))
    }
    it("Should EBind correctly evaluate") {
      val prog: SimpleEff[Id, Int, EList# ::|] = for {
        a <- pure(1)
        b <- pure(2)
        c <- pure(a + b)
      } yield c
      assertResult(3)(runPure(prog))
    }
    it("Should CallP correctly evaluate") {
      val prog: SimpleEff[Id, Int, EList# ::[TEST[Int]]# ::|] = for {
        a <- get()
        b <- pure(2)
        c <- pure(a + b)
      } yield c
      assertResult(3)(runPureInit(prog, 1 :: Init.empty))
    }
    it("Should LiftP correctly evaluate") {
      val prog1: SimpleEff[Id, Int, EList# ::|] = for {
        a <- pure(2)
      } yield a
      val prog2: SimpleEff[Id, Int, EList# ::[TEST[Unit]]# ::|] = for {
        a <- pure(1)
        b <- prog1 : SimpleEff[Id, Int, EList# ::[TEST[Unit]]# ::|]
        c <- pure(a + b)
      } yield c
      assertResult(3)(runPure(prog2))
    }
    it("Should New correctly evaluate") {
      val prog1: SimpleEff[Id, Int, EList# ::[TEST[Unit]]# ::|] = for {
        a <- pure(2)
      } yield a
      val prog2: SimpleEff[Id, Int, EList# ::|] = for {
        a <- pure(1)
        b <- newEff(effect[TEST[Unit]], (), prog1)
        c <- pure(a + b)
      } yield c
      assertResult(3)(runPure(prog2))
    }
    it("Should Label correctly evaluate") {
      trait L_1
      trait L_2

      val prog: SimpleEff[Id, Int, EList# ::[TEST[LRes[L_1, Int]]]# ::[TEST[LRes[L_2, Int]]]# ::|] = for {
        a <- label(lbl[L_1], get[Id, Int]()): SimpleEff[Id, Int, EList# ::[TEST[LRes[L_1, Int]]]# ::[TEST[LRes[L_2, Int]]]# ::|]
        b <- label(lbl[L_2], get[Id, Int]()): SimpleEff[Id, Int, EList# ::[TEST[LRes[L_1, Int]]]# ::[TEST[LRes[L_2, Int]]]# ::|]
        c <- pure(a + b)
      } yield c
      assertResult(3)(runPureInit(prog, LRes[L_1, Int](1) :: LRes[L_2, Int](2) :: Init.empty))
    }
  }
}

sealed trait TestEffect[A, RES_IN, RES_OUT] extends Effect[A, RES_IN, RES_OUT]
case class TestEffectGet[A]() extends TestEffect[A, A, A]

object TestEffect {
  implicit def testEffectHandlerInstance[M[_]]: Handler[M, TestEffect] = new Handler[M, TestEffect] {
    def handle[A, B, RES_IN, RES_OUT](resourceIn: RES_IN, effect: TestEffect[A, RES_IN, RES_OUT], f: A => RES_OUT => M[B]): M[B] = effect match {
      case effect: TestEffectGet[A] => f(resourceIn)(resourceIn)
    }
  }

  type TEST[A] = EFFECT[A, TestEffect]

  def get[M[_], A](): SimpleEff[M, A, EList# ::[TEST[A]]# ::|] =
    call(TestEffectGet(): TestEffect[A, A, A])
}
