package ru.spbau.jvm.scala.hlist

trait Splittable[
List <: HList,
Index <: Number,
ResultLeft <: HList,
ResultRight <: HList
] {
  def apply(list: List, index: Index): (ResultLeft, ResultRight)
}

object Splittable {

  implicit def zeroSplittable[
  List <: HList
  ]: Splittable[List, Zero, Nil, List] =
    (list: List, _: Zero) => (HNil, list)

  implicit def splittable[
  Head,
  List <: HList,
  Prev <: Number,
  ResultLeft <: HList,
  ResultRight <: HList
  ](implicit splittable: Splittable[List, Prev, ResultLeft, ResultRight])
  : Splittable[Head :: List, Succ[Prev], Head :: ResultLeft, ResultRight] =
    (list: Head :: List, index: Succ[Prev]) => {
      val HCons(head, tail) = list
      val Succ(prev) = index
      val (left, right) = splittable(tail, prev)
      (head :: left, right)
    }
}
