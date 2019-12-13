package ru.spbau.jvm.scala

trait CanZip[List1 <: HList, List2 <: HList, Result <: HList] {
  def apply(list1: List1, list2: List2): Result
}

object CanZip {
  implicit def nil1CanZip[List <: HList]: CanZip[Nil, List, Nil] = (_: Nil, _: List) => HNil

  implicit def nil2CanZip[List <: HList]: CanZip[List, Nil, Nil] = (_: List, _: Nil) => HNil

  implicit def canZip[Head1, Tail1 <: HList, Head2, Tail2 <: HList, Result <: HList]
    (implicit canZip: CanZip[Tail1, Tail2, Result]): CanZip[Head1 :: Tail1, Head2 :: Tail2, (Head1, Head2) :: Result] =
      (cons1: Head1 :: Tail1, cons2: Head2 :: Tail2) => {
        val HCons(head1, tail1) = cons1
        val HCons(head2, tail2) = cons2
        HCons((head1, head2), canZip(tail1, tail2))
      }
}
