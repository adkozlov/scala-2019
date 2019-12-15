package ru.spbau.jvm.scala

object Utils {

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
            CIndex <: Index,
            Result <: (HList, HList)
        ](index: CIndex)
         (implicit splittable: Splittable[List, CIndex, Result]): Result = splittable(list, index)

        def zip[
            Other <: HList,
            Result <: HList
        ](other: Other)
         (implicit zippable: Zippable[List, Other, Result]): Result = zippable(list, other)
    }

}
