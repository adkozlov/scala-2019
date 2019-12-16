package ru.spbau.jvm.scala.hlist


trait Zippable[Left <: HList, Right <: HList, Zipped <: HList] {
  def apply(first: Left, second: Right): Zipped
}

object Zippable {
  implicit def leftAndRightNil: Zippable[Nil, Nil, Nil] = (_: Nil, _: Nil) => HNil

  implicit def leftNil[Right <: HList]: Zippable[Nil, Right, Nil] = (_: Nil, _: Right) => HNil

  implicit def rightNil[Left <: HList]: Zippable[Left, Nil, Nil] = (_: Left, _: Nil) => HNil

  implicit def zippable[
    LeftHead,
    LeftTail <: HList,
    RightHead,
    RightTail <: HList,
    Zipped <: HList
  ](implicit zippable: Zippable[LeftTail, RightTail, Zipped]): Zippable[LeftHead :: LeftTail, RightHead :: RightTail, (LeftHead, RightHead) :: Zipped] = (left: LeftHead :: LeftTail, right: RightHead :: RightTail) => {
    val HCons(leftHead, leftTail) = left
    val HCons(rightHead, rightTail) = right
    HCons((leftHead, rightHead), zippable(leftTail, rightTail))
  }
}
