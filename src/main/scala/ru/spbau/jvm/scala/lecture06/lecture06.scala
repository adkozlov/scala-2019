package ru.spbau.jvm.scala

package object lecture06 {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def :::[
      Prefix <: HList,
      Result <: HList
    ](prefix: Prefix)
     (implicit appendable: Appendable[Prefix, List, Result]): Result =
      appendable(prefix, list)

    def zip[
      List2 <: HList,
      Result <: HList
    ](list2: List2)
     (implicit zippable: Zippable[List, List2, Result]): Result =
      zippable(list, list2)

    def splitAt[
      Index <: Nat,
      Result <: (HList, HList)
    ](index: Index)
     (implicit splittable: Splittable[List, Index, Result]): Result =
      splittable(list, index)
  }

}
