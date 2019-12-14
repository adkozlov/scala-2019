package ru.spbau.jvm.scala
package lecture06

trait Splittable[
  List <: HList,
  Index <: UInteger,
  Result <: (HList, HList)
] {
  def apply(list: List, index: Index): Result
}

object Splittable {

  implicit def zeroSplittable[List <: HList]: Splittable[List, Zero, (Nil, List)] =
    (list: List, _: Zero) => (HNil, list)

  implicit def splittable[
    Head,
    Tail <: HList,
    IntegerTale <: UInteger,
    ResultLeft <: HList,
    ResultRight <: HList
  ](implicit splittable: Splittable[Tail, IntegerTale, (ResultLeft, ResultRight)]):
  Splittable[Head :: Tail, Next[IntegerTale], (Head :: ResultLeft, ResultRight)] =
    (cons: Head :: Tail, index: Next[IntegerTale]) => {
      val HCons(head, tail) = cons
      val Next(integerTale) = index
      val (resultLeft, resultRight) = splittable(tail, integerTale)
      (head :: resultLeft, resultRight)
    }
}