package ru.spbau.jvm.scala.heterogeneous

sealed trait HList

object HList {

  case class HCons[+Head, +Tail <: HList](head: Head, tail: Tail) extends HList

  case object HNil extends HList

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[H](head: H): HCons[H, List] = HCons(head, list)

    def zip[List2 <: HList, Result <: HList](other: List2)(implicit zippable: Zippable[List, List2, Result]): Result =
      zippable(list, other)

    def splitAt[N <: NonNegative, LeftResult <: HList, RightResult <: HList](index: N)(implicit splittable: Splittable[N, List, LeftResult, RightResult]): (LeftResult, RightResult) =
      splittable(list, index)
  }

}