package ru.spbau.jvm.scala

package object hlist {
  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type
  type Zero = Zero.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {
    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def splitAt[Index <: Natural, Splitted <: (HList, HList)](index: Index)(implicit splittable: Splittable[Index, List, Splitted]): Splitted = splittable(index, list)

    def zip[Right <: HList, Zipped <: HList](other: Right)(implicit zippable: Zippable[List, Right, Zipped]): Zipped = zippable(list, other)
  }
}


