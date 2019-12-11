package ru.spbau.jvm.scala

package object lecture06 {

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

    def zip[
      Suffix <: HList,
      Result <: HList
    ](suffix: Suffix)
     (implicit zippable: Zippable[List, Suffix, Result]): Result =
      zippable(list, suffix)

    def splitAt[
      Index <: Number,
      Left <: HList,
      Right <: HList
    ](index: Index)
     (implicit splittable: Splittable[List, Index, Left, Right]): (Left, Right) =
      splittable(list, index)
  }

}
