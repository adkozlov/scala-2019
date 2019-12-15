package ru.spbau.jvm

package object scala {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type
  type Zero = Zero.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def zip[
      Other <: HList,
      Result <: HList
    ](other: Other)
     (implicit zippable: Zippable[List, Other, Result]): Result = zippable(list, other)

    def splitAt[
      Index <: Nat,
      Result <: (HList, HList)
    ](index: Index)
     (implicit splittable: Splittable[List, Index, Result]): Result = splittable(list, index)

  }

}
