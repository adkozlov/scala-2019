package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.Utils._

trait Splittable[
    List <: HList,
    CIndex <: Index,
    Result <: (HList, HList)
] {
    def apply(list: List, index: CIndex): Result
}

object Splittable {
    implicit def zeroSplittable[
        List <: HList
    ]: Splittable[List, Zero, (Nil, List)] = (list: List, _: Zero) => (HNil, list)

    implicit def splittable[
    Head,
    Tail <: HList,
    CIndex <: Index,
    FirstResult <: HList,
    SecondResult <: HList
    ](implicit splittable: Splittable[Tail, CIndex, (FirstResult, SecondResult)]):
    Splittable[Head :: Tail, Nat[CIndex], (Head :: FirstResult, SecondResult)] =
        (list: Head :: Tail, index: Nat[CIndex]) => {
            val HCons(head, tail) = list
            val (firstResult, secondResult) = splittable(tail, index.n)
            (HCons(head, firstResult), secondResult)
        }
}
