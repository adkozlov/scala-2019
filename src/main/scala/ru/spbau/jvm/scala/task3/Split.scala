package ru.spbau.jvm.scala.task3

trait Split[
  Index <: Nat,
  List <: HList,
  Left <: HList,
  Right <: HList,
] {
  def apply(index: Index, list: List): (Left, Right)
}

object Split {
  implicit def caseZero[List <: HList]: Split[Zero, List, Nil, List] =
    (_, list) => (HNil, list)

  implicit def caseSuc[Head, Tail <: HList, Index <: Nat, Left <: HList, Right <: HList]
  (implicit splittable: Split[Index, Tail, Left, Right]): Split[Suc[Index], Head :: Tail, Head :: Left, Right] =
    (index, list) => {
      val (left, right) = splittable(index.prev, list.tail)
      (list.head :: left, right)
    }
}