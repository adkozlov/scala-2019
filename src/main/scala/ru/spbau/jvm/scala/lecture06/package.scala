package ru.spbau.jvm.scala

package object lecture06 {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type
  type ZeroInd = Zero.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def :::[
      Prefix <: HList,
      Result <: HList
    ](prefix: Prefix)
     (implicit appendable: Appendable[Prefix, List, Result]): Result =
      appendable(prefix, list)

    def zip[
      OtherList <: HList,
      Result <: HList
    ](otherList: OtherList)
     (implicit zippable: Zippable[List, OtherList, Result]): Result =
      zippable(list, otherList)

    def splitAt[
      Index <: Natural,
      Result <: (HList, HList)
    ](index: Index)
     (implicit splittable: Splittable[List, Index, Result]): Result =
      splittable(list, index)
  }

}
