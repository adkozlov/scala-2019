package ru.spbau.jvm.scala.hlist

trait Zippable[
  FirstList <: HList,
  SecondList <: HList,
  Result <: HList] {
  def apply(first: FirstList, second: SecondList): Result
}

object Zippable {

  // Nil zip Nil = Nil
  implicit def nilZippable: Zippable[Nil, Nil, Nil] =
    (_: Nil, _: Nil) => HNil

  // Nil zip List = Nil
  implicit def nilLeftZippable[
    List <: HList
  ]: Zippable[Nil, List, Nil] = (_: Nil, _: List) => HNil

  // List zip Nil = Nil
  implicit def nilRightZippable[
    List <: HList
  ]: Zippable[List, Nil, Nil] = (_: List, _: Nil) => HNil

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