package ru.spbau.jvm

package object scala {
  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type

  type Z = Zero.type
  type S[Ind <: Nat] = Suc[Ind]

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {
    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def zip[List2 <: HList, Result <: HList](other: List2)(implicit canZip: CanZip[List, List2, Result]): Result =
      canZip(list, other)

    def splitAt[Ind <: Nat, ResultL <: HList, ResultR <: HList](index : Ind)
      (implicit canSplitAt: CanSplitAt[List, Ind, ResultL, ResultR]): (ResultL, ResultR)
        = canSplitAt(list, index)
  }
}
