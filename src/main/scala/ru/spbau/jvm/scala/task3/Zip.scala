package ru.spbau.jvm.scala.task3

trait Zip[
  Left <: HList,
  Right <: HList,
  Result <: HList
] {
  def apply(left: Left, right: Right): Result
}

object Zip {

  implicit def caseNilNil: Zip[Nil, Nil, Nil] = (_, _) => HNil

  implicit def caseConsNil[Left <: HList]: Zip[Left, Nil, Nil] = (_, _) => HNil

  implicit def caseNilCons[Right <: HList]: Zip[Nil, Right, Nil] = (_, _) => HNil

  implicit def caseConsCons[
    LeftHead,
    Left <: HList,
    RightHead,
    Right <: HList,
    Result <: HList
  ](implicit zipable: Zip[Left, Right, Result]): Zip[LeftHead :: Left, RightHead :: Right, (LeftHead, RightHead) :: Result] =
    (left, right) => (left.head, right.head) :: zipable(left.tail, right.tail)

}
