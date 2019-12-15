package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.ListUtils._

trait Splittable[
List <: HList,
SplitIndex <: UInt,
Result <: (HList, HList)
] {
  def apply(list: List, index: SplitIndex): Result
}

object Splittable {
  implicit def splitByZero[List <: HList]:
    Splittable[List, Nil, (HNil, List)] =
    (list: List, _: Nil) => (HNil, list)

  implicit def splitEmpty:
    Splittable[HNil, Nil, (HNil, HNil)] =
  (_: HNil, _: Nil) => (HNil, HNil)

  implicit def split[
  Head,
  List <: HList,
  Index <: UInt,
  Tail <: HList,
  HLeft <: HList,
  HRight <: HList]
    (implicit splittable: Splittable[Tail, Index, (HLeft, HRight)]):
    Splittable[Head :: Tail, Number[Index], (Head :: HLeft, HRight)] =
    (list: Head :: Tail, index: Number[Index]) => {
      val HCons(head, tail) = list
      val (left, right) = splittable(tail, index.tail)
      (head :: left, right)
    }
}