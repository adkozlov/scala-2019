package ru.spbau.jvm.scala.lecture06

trait SplitHelper[
  List <: HList,
  Index <: NonNegative,
  ResultBegin <: HList,
  ResultEnd <: HList
] {
  def apply(list: List): (ResultBegin, ResultEnd)
}

object SplitHelper {

  implicit def zeroSplit[List <: HList]: SplitHelper[List, Zero, Nil, List] = l => (HNil, l)

  implicit def regularSplit[
    Head,
    Tail <: HList,
    Prev <: NonNegative,
    TailResBegin <: HList,
    TailResEnd <: HList
  ](implicit splitHelper: SplitHelper[Tail, Prev, TailResBegin, TailResEnd]): SplitHelper[Head :: Tail, Next[Prev], Head :: TailResBegin, TailResEnd] =
    (list: Head :: Tail) => {
      val HCons(head, tail) = list
      val (resBegin, resEnd) = splitHelper(tail)
      (head :: resBegin, resEnd)
    }
}
