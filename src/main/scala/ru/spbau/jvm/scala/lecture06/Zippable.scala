package ru.spbau.jvm.scala.lecture06

trait Zippable[
  First <: HList,
  Second <: HList,
  Result <: HList
] {
  def apply(first: First, second: Second): Result
}

object Zippable {

  // Nil zip List = Nil
  implicit def firstNilZippable[
    List <: HList
  ]: Zippable[Nil, List, Nil] =
    (_: Nil, _: List) => HNil

  // HCons zip Nil = Nil
  implicit def secondNilZippable[
    List <: HCons[_, _]
  ]: Zippable[List, Nil, Nil] =
    (_: List, _: Nil) => HNil

  // Tail1 zip Tail2 = Result
  // (Head1 :: Tail1) zip (Head2 :: Tail2) = (Head1, Head2) :: Result
  implicit def zippable[
    Head1,
    Head2,
    Tail1 <: HList,
    Tail2 <: HList,
    TailResult <: HList
  ](implicit zippable: Zippable[Tail1, Tail2, TailResult]): Zippable[
    Head1 :: Tail1,
    Head2 :: Tail2,
    (Head1, Head2) :: TailResult
  ] = (first: Head1 :: Tail1, second: Head2 :: Tail2) => {
    val HCons(head1, tail1) = first
    val HCons(head2, tail2) = second
    (head1, head2) :: zippable(tail1, tail2)
  }
}