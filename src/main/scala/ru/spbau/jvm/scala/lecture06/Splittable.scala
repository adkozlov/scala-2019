package ru.spbau.jvm.scala.lecture06

trait Splittable[
  List <: HList,
  Index <: Natural,
  Result <: (HList, HList)
] {
  def apply(list: List, index: Index): Result
}

object Splittable {

  // List splitAt Zero = List
  implicit def zeroSplittable[
    List <: HList
  ]: Splittable[List, ZeroInd, (Nil, List)] =
    (list: List, _: ZeroInd) => (HNil, list)

  // Tail splitAt Prev = (First, Second)
  // (Head :: Tail) splitAt Succ(Prev) = (Head :: First, Second)
  implicit def splittable[
    Head,
    Tail <: HList,
    Prev <: Natural,
    First <: HList,
    Second <: HList
  ](implicit splittable: Splittable[Tail, Prev, (First, Second)]): Splittable[
    Head :: Tail,
    Succ[Prev],
    (Head :: First, Second)
  ] = (list: Head :: Tail, index: Succ[Prev]) => {
    val HCons(head, tail) = list
    val (first, second) = splittable(tail, index.prev)
    (head :: first, second)
  }
}
