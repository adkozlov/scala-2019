package ru.spbau.jvm.scala

trait Splittable[
  Index <: Nat,
  List <: HList,
  Left <: HList,
  Right <: HList,
] {
  def apply(index: Index, list: List): (Left, Right)
}

object Splittable {
  implicit def zeroSplittable[List <: HList]: Splittable[Zero.type, List, Nil, List] = (_: Zero.type, list: List) => (HNil, list)

  implicit def anySplittable[Index <: Nat, Head, Tail <: HList, Left <: HList, Right <: HList]
  (implicit splittable: Splittable[Index, Tail, Left, Right]): Splittable[Succ[Index], Head :: Tail, Head :: Left, Right] =
    (index: Succ[Index], list: Head :: Tail) => {
      val HCons(head, tail) = list
      val Succ(next) = index
      val (left, right) = splittable(next, tail)
      (HCons(head, left), right)
    }
}
