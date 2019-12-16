package ru.spbau.jvm.scala.hlist

trait Splittable[Index <: Natural, List <: HList, Splitted <: (HList, HList)] {
  def apply(index: Index, list: List): Splitted
}

object Splittable {
  implicit def zero[List <: HList]: Splittable[Zero, List, (Nil, List)] = (_: Zero, list: List) => (HNil, list)

  implicit def splittable[
    Head,
    Tail <: HList,
    Index <: Natural,
    Left <: HList,
    Right <: HList
  ](implicit splittable: Splittable[Index, Tail, (Left, Right)]): Splittable[Next[Index], Head :: Tail, (Head :: Left, Right)] = (index: Next[Index], list: Head :: Tail) => {
    val HCons(head, tail) = list
    val Next(prev) = index
    val (left, right) = splittable(prev, tail)
    (HCons(head, left), right)
  }
}
