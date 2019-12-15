package ru.spbau.jvm.scala.heterogeneous

import ru.spbau.jvm.scala.heterogeneous.NonNegative.Zero

trait Splittable[N <: NonNegative, List <: HList, LeftResult <: HList, RightResult <: HList] {
  def apply(list: List, n: N): (LeftResult, RightResult)
}

object Splittable {

  import HList._

  implicit def splittableZero[List <: HList]: Splittable[Zero.type, List, HNil.type, List] =
    (list: List, _: Zero.type) => (HNil, list)

  implicit def splittable[N <: NonNegative, List <: HList, Head, LeftResult <: HList, RightResult <: HList](implicit splittable: Splittable[N, List, LeftResult, RightResult]): Splittable[NonNegative.Next[N], HCons[Head, List], HCons[Head, LeftResult], RightResult] =
    (list: HCons[Head, List], n: NonNegative.Next[N]) => {
      val (left, right) = splittable(list.tail, n.tail)
      (list.head::left, right)
    }
}
