package ru.spbau.jvm

package object scala {
  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {
    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def splitAt[Index <: Nat, Left <: HList, Right <: HList](index: Index)(implicit splittable: Splittable[Index, List, Left, Right]) = splittable(index, list)

    def zip[Right <: HList, Both <: HList](right: Right)(implicit zippable: Zipable[List, Right, Both]) = zippable(list, right)
  }
}
