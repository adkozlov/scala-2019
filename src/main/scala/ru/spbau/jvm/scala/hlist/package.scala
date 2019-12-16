package ru.spbau.jvm.scala

package object hlist {

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
      SecondList <: HList,
      Result <: HList
    ](secondList: SecondList)
     (implicit zippable: Zippable[List, SecondList, Result]): Result =
      zippable(list, secondList)

    def splitAt[
      Index <: Nonnegative,
      Result <: (HList, HList)
    ](index: Index)
     (implicit splittable: Splittable[Index, List, Result]): Result =
      splittable(index, list)
  }
}
