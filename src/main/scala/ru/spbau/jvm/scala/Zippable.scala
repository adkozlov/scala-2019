package ru.spbau.jvm.scala

trait Zippable[
  Left <: HList,
  Right <: HList,
  Both <: HList
] {
  def apply(left: Left, right: Right): Both
}

object Zippable {
  implicit def bothNilZippable: Zippable[Nil, Nil, Nil] = (_: Nil, _: Nil) => HNil

  implicit def leftNilZippable[Right <: HList]: Zippable[Nil, Right, Nil] = (_: Nil, _: Right) => HNil

  implicit def rightNilZippable[Left <: HList]: Zippable[Left, Nil, Nil] = (_: Left, _: Nil) => HNil

  implicit def anyZippable[LeftHead, RightHead, LeftTail <: HList, RightTail <: HList, Both <: HList]
  (implicit zippable: Zippable[LeftTail, RightTail, Both]): Zippable[LeftHead :: LeftTail, RightHead :: RightTail, (LeftHead, RightHead) :: Both] =
    (left: LeftHead :: LeftTail, right: RightHead :: RightTail) => {
      val HCons(leftHead, leftTail) = left
      val HCons(rightHead, rightTail) = right
      HCons((leftHead, rightHead), zippable(leftTail, rightTail))
    }
}
