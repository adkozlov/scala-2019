package ru.spbau.jvm.scala

trait Splittable[
  List <: HList,
  Index <: Nat,
  Result <: (HList, HList)
] {
  def apply(list: List, index: Index): Result
}


object Splittable {

  implicit def zeroSplittable[
    List <: HList
  ]: Splittable[List, Zero, (Nil, List)] = (list: List, _: Zero) => (HNil, list)

  implicit def splittable[
    Head,
    Tail <: HList,
    Index <: Nat,
    Result1 <: HList,
    Result2 <: HList
  ](implicit splittable: Splittable[Tail, Index, (Result1, Result2)]):
  Splittable[Head :: Tail, Suc[Index], (Head :: Result1, Result2)] =
    (list: Head :: Tail, index: Suc[Index]) => {
      val HCons(head, tail) = list
      val (result1, result2) = splittable(tail, index.n)
      (HCons(head, result1), result2)
    }
}
