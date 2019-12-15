package ru.spbau.jvm.scala
import lecture06._

package object HList {
  implicit class HListZipSplitExt[List <: HList](private val list: List) extends AnyVal {

    def zip[
      List2 <: HList,
      Result <: HList
    ](list2: List2)
     (implicit zippable: Zippable[List, List2, Result]): Result =
      zippable(list, list2)

    def splitAt[
      Num <: MyNumber,
      Result1 <: HList,
      Result2 <: HList
    ](number: Num)
     (implicit splittable: Splittable[Num, List, Result1, Result2]): (Result1, Result2) =
      splittable(number, list)
  }
}
