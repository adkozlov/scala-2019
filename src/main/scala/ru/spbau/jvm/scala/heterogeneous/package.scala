package ru.spbau.jvm.scala

package object heterogeneous {
  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {
    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def zip[Second <: HList, Result <: HList]
    (snd: Second)
    (implicit zippable: Zippable[List, Second, Result]): Result = zippable(list, snd)

    def splitAt[Index <: Number, FResult <: HList, SResult <: HList]
    (index: Index)
    (implicit splittable: Splittable[List, Index, FResult, SResult]): (FResult, SResult) =
      splittable(list, index)
  }
}

