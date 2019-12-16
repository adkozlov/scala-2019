package ru.spbau.jvm.scala.hlist

trait Zippable[
First <: HList,
Second <: HList,
Result <: HList
] {
  def apply(first: First, second: Second): Result
}

object Zippable {

  implicit def nilFirstZippable[
  Second <: HList
  ]: Zippable[Nil, Second, Nil] =
    (_: Nil, _: Second) => HNil

  implicit def nilSecondZippable[
  Head,
  First <: HList
  ]: Zippable[Head :: First, Nil, Nil] =
    (_: Head :: First, _: Nil) => HNil

  implicit def zippable[
  HeadFirst,
  HeadSecond,
  First <: HList,
  Second <: HList,
  Result <: HList
  ](implicit zippable: Zippable[First, Second, Result])
  : Zippable[HeadFirst :: First, HeadSecond :: Second, (HeadFirst, HeadSecond) :: Result] =
    (first: HeadFirst :: First, second: HeadSecond :: Second) => {
      val HCons(headFirst, tailFirst) = first
      val HCons(headSecond, tailSecond) = second
      HCons((headFirst, headSecond), zippable(tailFirst, tailSecond))
    }
}
