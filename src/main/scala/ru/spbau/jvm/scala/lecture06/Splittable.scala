package ru.spbau.jvm.scala.lecture06

trait Splittable[
  List <: HList,
  Index <: Number,
  Left <: HList,
  Right <: HList
] {
  def apply(list: List, index: Index): (Left, Right)
}


object Splittable {

  implicit def zeroSplittable[
    Head,
    List <: HList
  ]: Splittable[Head :: List, Zero, Head :: Nil, List] =
    (cons: Head :: List, _: Zero) => {
      val HCons(head, list) = cons
      (head :: HNil, list)
    }

  implicit def splittable[
    Head,
    List <: HList,
    Tail <: Number,
    List1 <: HList,
    List2 <: HList
  ](implicit splittable: Splittable[List, Tail, List1, List2])
  : Splittable[Head :: List, PlusOne[Tail], Head :: List1, List2] =
    (cons: Head :: List, index: PlusOne[Tail]) => {
      val HCons(head, prefix) = cons
      val PlusOne(tail) = index
      val (list1, list2) = splittable(prefix, tail)
      (head :: list1, list2)
    }


}

