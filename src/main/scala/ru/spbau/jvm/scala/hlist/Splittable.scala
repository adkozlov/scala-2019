package ru.spbau.jvm.scala.hlist

trait Splittable[
  Index <: Nonnegative,
  List <: HList,
  Result <: (HList, HList)
] {
  def apply(index: Index, list: List): Result
}

object Splittable {

  // List splitAt Zero = (Nil, List)
  implicit def zeroSplittable[
    List <: HList
  ]: Splittable[Zero.type, List, (Nil, List)] = (_: Zero.type, list: List) => (HNil, list)

  implicit def splittable[
    Index <: Nonnegative,
    Head,
    Tail <: HList,
    FirstSplit <: HList,
    SecondSplit <: HList
  ](implicit splittable: Splittable[Index, Tail, (FirstSplit, SecondSplit)]):
  Splittable[Succ[Index], Head :: Tail, (Head :: FirstSplit, SecondSplit)] =
    (index: Succ[Index], cons: Head :: Tail) => {
      val HCons(head, tail) = cons
      val (firstSplitted, secondSplitted) = splittable(index.minusOne, tail)
      (HCons(head, firstSplitted), secondSplitted)
    }
}