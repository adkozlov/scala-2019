package ru.spbau.jvm.scala

object ListUtils {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type HNil = HNil.type
  type Nil = Nil.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def :::[
    Prefix <: HList,
    Result <: HList
    ](prefix: Prefix)
     (implicit appendable: Appendable[Prefix, List, Result]): Result =
      appendable(prefix, list)

    def split[
    Index <: UInt,
    Result <: (HList, HList)
    ] (index: Index)
      (implicit splittable: Splittable[List, Index, Result]): Result =
      splittable(list, index)

    def zip[
    AnotherList <: HList,
    Result <: HList
    ] (anotherList: AnotherList)
      (implicit  zippable: Zippable[List, AnotherList, Result]): Result =
      zippable(list, anotherList)
  }
}
