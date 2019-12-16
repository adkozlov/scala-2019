package ru.spbau.jvm.scala
package lecture06

trait Splittable[
  List <: HList,
  Index <: Nat,
  Result <: (HList, HList)
] {
  def apply(list: List, index: Index): Result
}

object Splittable {

  implicit def splittable0[
    List <: HList
  ]: Splittable[List, Nat0.type, (Nil, List)] =
    (list: List, _: Nat0.type) => (HNil, list)

  implicit def splittable[
    Head,
    Prefix <: HList,
    Index <: Nat,
    Result1 <: HList,
    Result2 <: HList
  ](implicit splittable: Splittable[Prefix, Index, (Result1, Result2)]): Splittable[Head :: Prefix, Succ[Index], (Head :: Result1, Result2)] =
    (cons: Head :: Prefix, index: Succ[Index]) => {
      val prev = index.prev
      val HCons(head, prefix) = cons
      val (first, second) = splittable(prefix, prev)
      (head :: first, second)
    }
}