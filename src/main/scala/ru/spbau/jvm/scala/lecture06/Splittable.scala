package ru.spbau.jvm.scala
package lecture06

trait Splittable[List <: HList, Ind <: Index, Result <: (HList, HList)] {
  def apply(list: List, index: Ind): Result
}

object Splittable {

  implicit def zeroSplittable[List <: HList]
    : Splittable[List, NilIndex.type, (Nil, List)] =
    (list: List, _: NilIndex.type) => (HNil, list)

  implicit def splittable[Head,
                          List <: HList,
                          Ind <: Index,
                          ResultLeft <: HList,
                          ResultRight <: HList](
    implicit splittable: Splittable[List, Ind, (ResultLeft, ResultRight)]
  ): Splittable[Head :: List, AtLeastOneIndex[Ind], (Head :: ResultLeft, ResultRight)] =
    (list: Head :: List, index: AtLeastOneIndex[Ind]) => {
      val HCons(head, tail) = list
      val (left, right) = splittable.apply(tail, index.remains)
      (head :: left, right)
    }
}
