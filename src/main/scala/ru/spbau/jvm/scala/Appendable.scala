package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.ListUtils._

trait Appendable[
Prefix <: HList,
List <: HList,
Result <: HList
] {
  def apply(prefix: Prefix, list: List): Result
}

object Appendable {

  // HNil ::: List = List
  implicit def nilAppendable[
  List <: HList
  ]: Appendable[HNil, List, List] =
    (_: HNil, list: List) => list

  // Prefix ::: List = Result
  // (Head :: Prefix) ::: List = Head :: Result
  implicit def appendable[
  Head,
  Prefix <: HList,
  List <: HList,
  Result <: HList
  ](implicit appendable: Appendable[Prefix, List, Result]):
    Appendable[Head :: Prefix, List, Head :: Result] =
    (cons: Head :: Prefix, list: List) => {
      val HCons(head, prefix) = cons
      HCons(head, appendable(prefix, list))
    }
}