package ru.spbau.jvm.scala

package object lecture06 {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type
  type Zero = NZero.type

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def :::[
      Prefix <: HList,
      Result <: HList
    ](prefix: Prefix)
     (implicit appendable: Appendable[Prefix, List, Result]): Result =
      appendable(prefix, list)

    def zip[
      Right <: HList,
      Result <: HList
    ](right: Right)
     (implicit zipHelper: ZipHelper[List, Right, Result]): Result =
      zipHelper(list, right)

    def splitAt[
      Index <: NonNegative,
      ResultBegin <: HList,
      ResultEnd <: HList
    ](nonNegative: Index)
     (implicit splitHelper: SplitHelper[List, Index, ResultBegin, ResultEnd]): (ResultBegin, ResultEnd) =
      splitHelper(list)
  }
}
