package ru.spbau.jvm.scala
package lecture06

trait Zippable[
  List <: HList,
  Other <: HList,
  Result <: HList
] {
  def apply(list: List, other: Other): Result
}

object Zippable {

  // Nil zip Nil = Nil
  implicit def nilZippableNil: Zippable[Nil, Nil, Nil] =
    (_: Nil, _: Nil) => HNil

  // List zip Nil = Nil
  implicit def nilZippable[
    List <: HList
  ]: Zippable[List, Nil, Nil] =
    (_: List, _: Nil) => HNil

  // Nil zip List = Nil
  implicit def zippableNil[
    List <: HList
  ]: Zippable[Nil, List, Nil] =
    (_: Nil, _: List) => HNil

  // List zip Other = Result
  // Head :: List zip OtherHead :: Other = (Head, OtherHead) :: Result
  implicit def zippable[
    Head,
    List <: HList,
    OtherHead,
    Other <: HList,
    Result <: HList
  ](implicit zippable: Zippable[List, Other, Result]): Zippable[Head :: List, OtherHead :: Other, (Head, OtherHead) :: Result] =
    (list: Head :: List, other: OtherHead :: Other) => {
      val HCons(head, tail) = list
      val HCons(otherHead, otherTail) = other
      (head, otherHead) :: zippable(tail, otherTail)
    }
}
