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
  implicit def nilZippableLeft[
    List <: HList
  ]: Zippable[Nil, List, Nil] =
    (_: Nil, _: List) => HNil

  implicit def nilZipppableRight[
    List <: HList
  ]: Zippable[List, Nil, Nil] =
    (_: List, _: Nil) => HNil

  implicit def nilZippableBoth: Zippable[Nil, Nil, Nil] =
    (_: Nil, _: Nil) => HNil

  implicit def zippable[
    HeadFirst,
    First <: HList,
    HeadSecond,
    Second <: HList,
    Result <: HList
  ](implicit zippable: Zippable[First, Second, Result]): Zippable[HeadFirst :: First, HeadSecond :: Second, (HeadFirst, HeadSecond) :: Result] =
    (consA: HeadFirst :: First, consB: HeadSecond :: Second) => {
      val HCons(headFirst, first) = consA
      val HCons(headSecond, second) = consB
      (headFirst, headSecond) :: zippable(first, second)
    }
}