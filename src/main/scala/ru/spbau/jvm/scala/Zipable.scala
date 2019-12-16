package ru.spbau.jvm.scala

trait Zipable[
  Left <: HList,
  Right <: HList,
  Result <: HList] {
  def apply(lhs: Left, rhs: Right): Result
}

object Zipable {
  implicit def zipableLeftNil[Right <: HList]: Zipable[Nil, Right, Nil] = (_: Nil, _: Right) => HNil

  implicit def zipableRightNil[Left <: HList]: Zipable[Left, Nil, Nil] = (_: Left, _: Nil) => HNil

  implicit def zipableLeftRightNil: Zipable[Nil, Nil, Nil] = (_: Nil, _: Nil) => HNil

  implicit def zipable[HeadL, HeadR, TailL <: HList, TailR <: HList, Result <: HList]
  (implicit zipable: Zipable[TailL, TailR, Result]):
  Zipable[HeadL :: TailL, HeadR :: TailR, (HeadL, HeadR) :: Result] =
    (left: HeadL :: TailL, right: HeadR :: TailR) => {
      val HCons(lHead, lTail) = left
      val HCons(rHead, rTail) = right
      HCons((lHead, rHead), zipable(lTail, rTail))
    }


}
