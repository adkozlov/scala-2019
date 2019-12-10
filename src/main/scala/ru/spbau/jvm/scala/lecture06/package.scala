package ru.spbau.jvm.scala

package object lecture06 {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type

  type _0 = Zero.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def :::[
      Prefix <: HList,
      Result <: HList
    ](prefix: Prefix)
     (implicit appendable: Appendable[Prefix, List, Result]): Result =
      appendable(prefix, list)

    def zip[
      Other <: HList,
      Result <: HList
    ](other: Other)
     (implicit zippable: Zippable[List, Other, Result]): Result =
      zippable(list, other)

    def splitAt[
      N <: Nat,
      Left <: HList,
      Right <: HList
    ](nat: N)
     (implicit splittable: Splittable[List, N, Left, Right]): (Left, Right) =
      splittable(list, nat)
  }

}
