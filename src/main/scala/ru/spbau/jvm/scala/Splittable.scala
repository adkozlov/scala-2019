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
  type Zero = Zero.type

  implicit def splittableZero[List <: HList]: Splittable[Zero, List, Nil, List] = (_: Zero, list: List) => (HNil, list)

  implicit def splittable[Index <: Nat, Head, Tail <: HList, PartL <: HList, PartR <: HList]
  (implicit splittable: Splittable[Index, Tail, PartL, PartR]): Splittable[Suc[Index], Head :: Tail, Head :: PartL, PartR] =
    (curIndex: Suc[Index], list: Head :: Tail) => {
      val HCons(head, tail) = list
      val Suc(next) = curIndex
      val (left, right) = splittable(next, tail)
      (HCons(head, left), right)
    }
}
