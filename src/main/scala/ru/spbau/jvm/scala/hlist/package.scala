package ru.spbau.jvm.scala

package object hlist {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type
  type Zero = Zero.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def :::[
      Prefix <: HList,
      Result <: HList
    ](prefix: Prefix)
     (implicit appendable: Appendable[Prefix, List, Result]): Result =
      appendable(prefix, list)

    def splitAt[
    Index <: Number,
    ResultLeft <: HList,
    ResultRight <: HList
    ](index: Index)
     (implicit splittable: Splittable[List, Index, ResultLeft, ResultRight]): (ResultLeft, ResultRight) =
      splittable(list, index)

    def zip[
      Second <: HList,
      Result <: HList
    ](second: Second)
     (implicit zippable: Zippable[List, Second, Result]): Result =
      zippable(list, second)
  }
}
