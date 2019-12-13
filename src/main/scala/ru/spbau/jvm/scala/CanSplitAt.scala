package ru.spbau.jvm.scala

trait CanSplitAt[List <: HList, Ind <: Nat, ResultL <: HList, ResultR <: HList] {
  def apply(list: List, index: Ind): (ResultL, ResultR)
}

object CanSplitAt {
  implicit def nilListSplitAt: CanSplitAt[Nil, _, Nil, Nil] = (_, _) => (HNil, HNil)

  implicit def zeroIndSplitAt[List <: HList]: CanSplitAt[List, Z, Nil, List] = (list, _) => (HNil, list)

  implicit def consListSucIndSplitAt[Head, Tail <: HList, Ind <: Nat, ResultL <: HList, ResultR <: HList]
    (implicit canSplitAt: CanSplitAt[Tail, Ind, ResultL, ResultR]): CanSplitAt[Head :: Tail, S[Ind], Head :: ResultL, ResultR] =
      (cons, index) => {
        val HCons(head, tail) = cons
        val Suc(indexNew) = index
        val (splitL, splitR) = canSplitAt(tail, indexNew)
        (HCons(head, splitL), splitR)
      }
}