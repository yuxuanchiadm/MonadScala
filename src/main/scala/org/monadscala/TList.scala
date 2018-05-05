package org.monadscala

import scala.language.existentials
import scala.language.higherKinds
import java.util.SubList

sealed trait TList[A] {
  trait TCons[H <: A, T <: TList[A]] extends TList[A]
  trait TNil extends TList[A]
}

object TList {
  sealed trait Elem[E, L <: TList[_]]
  case class ElemHere[A, E <: A, T <: TList[A]]() extends Elem[E, TList[A]#TCons[E, T]]
  case class ElemThere[A, E <: A, H <: A, T <: TList[A]](elem: Elem[E, T]) extends Elem[E, TList[A]#TCons[H, T]]

  implicit def implicitElemHere[A, E <: A, T <: TList[A]]: Elem[E, TList[A]#TCons[E, T]] = ElemHere()
  implicit def implicitElemThere[A, E <: A, H <: A, T <: TList[A]](implicit elem: Elem[E, T]): Elem[E, TList[A]#TCons[H, T]] = ElemThere(elem)

  sealed trait SubList[L1 <: TList[_], L2 <: TList[_]]
  case class SubListNil[A, L2 <: TList[A]]() extends SubList[TList[A]#TNil, L2]
  case class SubListIn[A, H <: A, L1 <: TList[A], L2 <: TList[A]](elem: Elem[H, L2], subList: SubList[L1, L2]) extends SubList[TList[A]#TCons[H, L1], L2]

  implicit def implicitSubListNil[A, L2 <: TList[A]]: SubList[TList[A]#TNil, L2] = SubListNil()
  implicit def implicitSubListIn[A, H <: A, L1 <: TList[A], L2 <: TList[A]](implicit elem: Elem[H, L2], subList: SubList[L1, L2]): SubList[TList[A]#TCons[H, L1], L2] = SubListIn(elem, subList)

  type Make[A] = {
    type TCons[H <: A, T <: TList[A]] = TList[A]#TCons[H, T]
    type TNil = TList[A]#TNil

    type ::| = TNil
    type ::[T1 <: A] = {
      type ::| = TCons[T1, TNil]
      type ::[T2 <: A] = {
        type ::| = TCons[T1, TCons[T2, TNil]]
        type ::[T3 <: A] = {
          type ::| = TCons[T1, TCons[T2, TCons[T3, TNil]]]
          type ::[T4 <: A] = {
            type ::| = TCons[T1, TCons[T2, TCons[T3, TCons[T4, TNil]]]]
            type ::[T5 <: A] = {
              type ::| = TCons[T1, TCons[T2, TCons[T3, TCons[T4, TCons[T5, TNil]]]]]
              type ::[T6 <: A] = {
                type ::| = TCons[T1, TCons[T2, TCons[T3, TCons[T4, TCons[T5, TCons[T6, TNil]]]]]]
                type ::[T7 <: A] = {
                  type ::| = TCons[T1, TCons[T2, TCons[T3, TCons[T4, TCons[T5, TCons[T6, TCons[T7, TNil]]]]]]]
                }
              }
            }
          }
        }
      }
    }
  }
}
