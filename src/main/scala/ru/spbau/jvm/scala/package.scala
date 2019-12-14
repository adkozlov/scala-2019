package ru.spbau.jvm

import ru.spbau.jvm.scala.HList.HCons

package object scala {
  type Nil = HList.HNil.type
  type Cons[Head, Tail <: HList] = HList.HCons[Head, Tail]

  type Z = Peano.Z.type
  type S[Idx <: Peano] = Peano.S[Idx]

  implicit class HListZS[List <: HList](private val list: List) extends AnyVal {
    def :::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def zip[OtherList <: HList, Res <: HList](
      other: OtherList
    )(implicit zip: Zip[List, OtherList, Res]): Res =
      zip(list, other)

    def splitAt[Idx <: Peano, Fir <: HList, Sec <: HList](index: Idx)(
      implicit splitAt: SplitAt[List, Idx, Fir, Sec]
    ): (Fir, Sec) = splitAt(list, index)
  }
}
