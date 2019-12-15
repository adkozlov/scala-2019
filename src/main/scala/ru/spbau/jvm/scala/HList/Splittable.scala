package ru.spbau.jvm.scala.HList

import ru.spbau.jvm.scala.lecture06._

trait Splittable[
  Num <: MyNumber,
  List <: HList,
  Result1 <: HList,
  Result2 <: HList
] {
  def apply(number: Num, list: List): (Result1, Result2)
}

object Splittable {
  // list splitAt 0 = (Nil, list)
  implicit def zeroSplittable[
    List <: HList
  ]: Splittable[Zero.type, List, Nil, List] =
    (_: Zero.type, list : List) => (HNil, list)

  implicit def splittable[
    Head,
    Tail <: HList,
    DecNum <: MyNumber,
    Result1 <: HList,
    Result2 <: HList
  ](implicit splittable: Splittable[DecNum, Tail, Result1, Result2]): Splittable[Inc[DecNum], Head :: Tail, Head :: Result1, Result2] =
    (number: Inc[DecNum], list: Head :: Tail) => {
      val dec = number.dec
      val HCons(head, tail) = list
      val (r1, t) = splittable(dec, tail)
      (head :: r1, t)
    }
}
