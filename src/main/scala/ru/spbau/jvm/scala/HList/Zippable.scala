package ru.spbau.jvm.scala
package HList
import lecture06._

trait Zippable[
  List1 <: HList,
  List2 <: HList,
  Result <: HList
] {
  def apply(prefix: List1, list: List2): Result
}

object Zippable {

  // Nil zip List = Nil
  implicit def nilZippableLeft[
    List <: HList
  ]: Zippable[Nil, List, Nil] =
    (_: Nil, _: List) => HNil

  // List zip Nil = Nil
  implicit def nilZippableRight[
    Head,
    Tail <: HList
  ]: Zippable[Head :: Tail, Nil, Nil] =
    (_: Head :: Tail, _: Nil) => HNil

  // Prefix1 zip Prefix2 = Result
  // (Head1 :: Prefix1) zip (Head2 :: Prefix2) = (Head1, Head2) :: Result
  implicit def zippable[
    Head1,
    Head2,
    Tail1 <: HList,
    Tail2 <: HList,
    Result <: HList
  ](implicit zippable: Zippable[Tail1, Tail2, Result]): Zippable[Head1 :: Tail1, Head2 :: Tail2, (Head1, Head2) :: Result] =
    (cons1: Head1 :: Tail1, cons2: Head2 :: Tail2) => {
      val HCons(head1, tail1) = cons1
      val HCons(head2, tail2) = cons2
      (head1, head2) :: zippable(tail1, tail2)
    }
}
