package ru.spbau.jvm.scala.heterogeneous

trait Splittable[
List <: HList,
Index <: Number,
FResult <: HList,
SResult <: HList
] {
  def apply(lst: List, index: Index): (FResult, SResult)

}

object Splittable {
  implicit def nilZeroSplittable: Splittable[Nil, Zero.type, Nil, Nil] =
    (lst: Nil, number: Zero.type) => (HNil, HNil)

  implicit def zeroSplittable[List <: HList]: Splittable[List, Zero.type, Nil, List] =
    (lst: List, number: Zero.type) => (HNil, lst)

  implicit def splittable[
    Head,
    Tail <: HList,
    PredNumber <: Number,
    FTail <: HList,
    SPart <: HList
  ](implicit splittable: Splittable[Tail, PredNumber, FTail, SPart])
  : Splittable[Head :: Tail, Succ[PredNumber], Head :: FTail, SPart] =
    (lst: Head :: Tail, number: Succ[PredNumber]) => {
      val HCons(head, tail) = lst
      val tailSplit = splittable(tail, number.pred)
      (HCons(head, tailSplit._1), tailSplit._2)
    }
}