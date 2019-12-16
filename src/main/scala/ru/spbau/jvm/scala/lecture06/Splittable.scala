package ru.spbau.jvm.scala.lecture06

trait Splittable[
  Index <: Nonnegative,
  List <: HList,
  Result <: (HList, HList)
] {
  def apply(index: Index, list: List): Result
}

object Splittable {
  implicit def splittableZero[
    List <: HList
  ]: Splittable[Zero.type, List, (Nil, List)] =
    (_: Zero.type, list: List) => (HNil, list)

  implicit def splittable[
    Index <: Nonnegative,
    Head,
    List <: HList,
    Left <: HList,
    Right <: HList
  ](implicit splittable: Splittable[Index, List, (Left, Right)])
  : Splittable[Next[Index], Head :: List, (Head :: Left, Right)] =
    (index: Next[Index], cons: Head :: List) => {
      val HCons(head, list) = cons
      val (left, right) = splittable(index.prev, list)
      (head :: left, right)
    }
}
