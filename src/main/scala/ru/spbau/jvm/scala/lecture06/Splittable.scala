package ru.spbau.jvm.scala.lecture06

trait Splittable[
  Index <: Nat,
  List <: HList,
  Result <: (HList, HList)
] {
  def apply(index: Index, list: List): Result
}

object Splittable {

  // List.spitAt(Zero) = (Nil, List)
  implicit def nilSplittable[List <: HList]: Splittable[Zero, List, (Nil, List)] =
    (_: Zero, list: List) => (HNil, list)

  implicit def splittable[
    Index <: Nat,
    Head,
    List <: HList,
    LResult <: HList,
    RResult <: HList,
    Result <: (HList, HList)
  ](implicit splittable: Splittable[Index, List, (LResult, RResult)]): Splittable[Succ[Index], Head :: List, (Head :: LResult, RResult)] =
    (index: Succ[Index], list: Head :: List) => {
      val HCons(head, tail) = list
      val Succ(prev) = index
      val (left, right) = splittable(prev, tail)
      (head :: left, right)
    }
}
