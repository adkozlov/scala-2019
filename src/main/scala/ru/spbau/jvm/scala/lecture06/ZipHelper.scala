package ru.spbau.jvm.scala
package lecture06


trait ZipHelper[
  Left <: HList,
  Right <: HList,
  Result <: HList
] {
  def apply(left: Left, right: Right): Result
}

object ZipHelper {

  implicit def leftNilZipper[Right <: HList]: ZipHelper[Nil, Right, Nil] = (_, _) => HNil

  implicit def rightNilZipper[Left <: HCons[_, _]]: ZipHelper[Left, Nil, Nil] = (_, _) => HNil

  implicit def zipper[
    LeftHead, RightHead,
    LeftTail <: HList, RightTail <: HList,
    TailResult <: HList
  ](implicit zipHelper: ZipHelper[LeftTail, RightTail, TailResult]): ZipHelper[LeftHead :: LeftTail, RightHead :: RightTail, (LeftHead, RightHead) :: TailResult] =
    (left: LeftHead :: LeftTail, right: RightHead :: RightTail) => {
      val HCons(leftHead, leftTail) = left
      val HCons(rightHead, rightTail) = right
      HCons((leftHead, rightHead), zipHelper(leftTail, rightTail))
    }
}