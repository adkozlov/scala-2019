package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.Utils._

trait Zippable[
    First <: HList,
    Second <: HList,
    Result <: HList
] {
    def apply(first: First, second: Second): Result
}

object Zippable {
    implicit def firstNilZippable[List <: HList]: Zippable[Nil, List, Nil] = (_: Nil, _: List) => HNil

    implicit def secondNilZippable[List <: HList]: Zippable[List, Nil, Nil] = (_: List, _: Nil) => HNil

    implicit def zippable[
        FirstHead,
        SecondHead,
        FirstTail <: HList,
        SecondTail <: HList,
        Result <: HList
    ](implicit zippable: Zippable[FirstTail, SecondTail, Result]):
    Zippable[FirstHead :: FirstTail, SecondHead :: SecondTail, (FirstHead, SecondHead) :: Result] =
        (first: FirstHead :: FirstTail, second: SecondHead :: SecondTail) => {
            val HCons(firstHead, firstTail) = first
            val HCons(secondHead, secondTail) = second
            HCons((firstHead, secondHead), zippable(firstTail, secondTail))
        }
}