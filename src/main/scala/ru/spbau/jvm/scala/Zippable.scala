package ru.spbau.jvm.scala

trait Zippable[
  List1 <: HList,
  List2 <: HList,
  Result <: HList
] {
  def apply(list1: List1, list2: List2): Result
}

object Zippable {

  implicit def leftNilZippable[
    List <: HList
  ]: Zippable[Nil, List, Nil] = (_: Nil, _: List) => HNil

  implicit def rightNilZippable[
    Head,
    Tail <: HList
  ]: Zippable[Head :: Tail, Nil, Nil] = (_: Head :: Tail, _: Nil) => HNil

  implicit def zippable[
    Head1,
    Head2,
    Tail1 <: HList,
    Tail2 <: HList,
    Result <: HList
  ](implicit zippable: Zippable[Tail1, Tail2, Result]):
  Zippable[Head1 :: Tail1, Head2 :: Tail2, (Head1, Head2) :: Result] =
    (list1: Head1 :: Tail1, list2: Head2 :: Tail2) => {
      val HCons(head1, tail1) = list1
      val HCons(head2, tail2) = list2
      HCons((head1, head2), zippable(tail1, tail2))
    }

}
