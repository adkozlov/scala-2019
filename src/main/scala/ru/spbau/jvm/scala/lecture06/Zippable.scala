package ru.spbau.jvm.scala
package lecture06

trait Zippable[
  First <: HList,
  Second <: HList,
  Result <: HList
] {
  def apply(first: First, second: Second): Result
}

object Zippable {

  implicit def firstNilZippable[Second <: HList]: Zippable[Nil, Second, Nil] =
    (_: Nil, _: Second) => HNil

  implicit def SecondNilZippable[
    FirstHead,
    FirstTail <: HList]: Zippable[FirstHead :: FirstTail, Nil, Nil] =
    (_ : FirstHead :: FirstTail, _: Nil) => HNil

  implicit def zippable[
    FirstHead,
    SecondHead,
    FirstTail <: HList,
    SecondTail <: HList,
    Result <: HList
  ](implicit zippable: Zippable[FirstTail, SecondTail, Result]):
  Zippable[FirstHead :: FirstTail, SecondHead :: SecondTail, (FirstHead, SecondHead) :: Result] =
    (firstCons: FirstHead :: FirstTail, secondCons: SecondHead :: SecondTail) => {
      val HCons(firstHead, firstTail) = firstCons
      val HCons(secondHead, secondTail) = secondCons
      HCons((firstHead, secondHead), zippable(firstTail, secondTail))
    }
}