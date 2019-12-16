package ru.spbau.jvm.scala

package object task3 {

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
      Index <: Nat,
      Left <: HList,
      Right <: HList
    ](index: Index)
     (implicit splittable: Split[Index, List, Left, Right]): (Left, Right) =
      splittable(index, list)

    def zip[Right <: HList, Result <: HList](right: Right)(implicit zipable: Zip[List, Right, Result]): Result =
      zipable(list, right)
  }

}
