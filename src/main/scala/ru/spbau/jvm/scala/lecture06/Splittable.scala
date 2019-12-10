package ru.spbau.jvm.scala
package lecture06

trait Splittable[
  List <: HList,
  N <: Nat,
  Prefix <: HList,
  Suffix <: HList
] {
  def apply(list: List, nat: N): (Prefix, Suffix)
}

object Splittable {

  // List splitAt Zero = (Nil, List)
  implicit def splittableAtZero[
    List <: HList
  ]: Splittable[List, _0, Nil, List] =
    (list: List, _: _0) => (HNil, list)

  // List splitAt Nat = (Prefix, Suffix)
  // (Head :: List) splitAt Succ(Nat) = (Head :: Prefix, Suffix)
  implicit def splittable[
    List <: HList,
    N <: Nat,
    Head,
    Prefix <: HList,
    Suffix <: HList
  ](implicit splittable: Splittable[List, N, Prefix, Suffix]): Splittable[HCons[Head, List], Succ[N], HCons[Head, Prefix], Suffix] =
    (cons: HCons[Head, List], succ: Succ[N]) => {
      val HCons(head, tail) = cons
      val Succ(nat) = succ
      val (prefix, suffix) = splittable(tail, nat)
      (HCons(head, prefix), suffix)
    }
}