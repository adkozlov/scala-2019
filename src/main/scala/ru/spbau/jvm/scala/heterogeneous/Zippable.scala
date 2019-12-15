package ru.spbau.jvm.scala.heterogeneous

trait Zippable[Left <: HList, Right <: HList, Result <: HList] {
  def apply(left: Left, right: Right): Result
}

object Zippable {

  import HList._

  implicit def zippableNilLeft[Right <: HList]: Zippable[HNil.type, Right, HNil.type] =
    (_: HNil.type, _: Right) => HNil

  implicit def zippableNilRight[Left <: HList, LeftHead]: Zippable[HCons[LeftHead, Left], HNil.type, HNil.type] =
    (_: HCons[LeftHead, Left], _: HNil.type) => HNil

  implicit def zippable[Left <: HList, Right <: HList, LeftHead, RightHead, Result <: HList](implicit zippable: Zippable[Left, Right, Result]): Zippable[HCons[LeftHead, Left], HCons[RightHead, Right], HCons[(LeftHead, RightHead), Result]] =
    (left: HCons[LeftHead, Left], right: HCons[RightHead, Right]) => HCons((left.head, right.head), zippable(left.tail, right.tail))
}