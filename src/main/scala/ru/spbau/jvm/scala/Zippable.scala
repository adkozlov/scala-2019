package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.ListUtils._

trait Zippable[
  First <: HList,
  Second <: HList,
  Result <: HList
  ] {
    def apply(first: First, second: Second): Result
  }

object Zippable {
  implicit def zipFirstNil[List <: HList]:
    Zippable[HNil, List, HNil] =
    (_: HNil, _: List) => HNil

  implicit def zipSecondNil[List <: HList]:
    Zippable[List, HNil, HNil] =
    (_: List, _: HNil) => HNil

  implicit def zipNils:
    Zippable[HNil, HNil, HNil] =
    (_: HNil, _: HNil) => HNil

  implicit def zip[
  FirstHead,
  SecondHead,
  FirstTail <: HList,
  SecondTail <: HList,
  Result <: HList]
  (implicit zippable: Zippable[FirstTail, SecondTail, Result]):
  Zippable[FirstHead :: FirstTail, SecondHead :: SecondTail,
    (FirstHead, SecondHead) :: Result] =
    (first: FirstHead :: FirstTail, second: SecondHead :: SecondTail) => {
      val HCons(firstHead, firstTail) = first
      val HCons(secondHead, secondTail) = second
      (firstHead, secondHead) :: zippable(firstTail, secondTail)
    }
}
