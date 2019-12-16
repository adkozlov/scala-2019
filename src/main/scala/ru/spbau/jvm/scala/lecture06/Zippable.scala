package ru.spbau.jvm.scala
package lecture06

trait Zippable[
  List1 <: HList,
  List2 <: HList,
  Result <: HList
] {
  def apply(list1: List1, list2: List2): Result
}

object Zippable {

  implicit def nilZippable[
    List <: HList
  ]: Zippable[Nil, List, Nil] =
    (_: Nil, _: List) => HNil

  implicit def zippableNil[
    Head,
    Prefix <: HList
  ]: Zippable[Head :: Prefix, Nil, Nil] =
    (_: Head :: Prefix, _: Nil) => HNil

  implicit def zippable[
    Head1,
    Prefix1 <: HList,
    Head2,
    Prefix2 <: HList,
    Result <: HList
  ](implicit zippable: Zippable[Prefix1, Prefix2, Result]): Zippable[Head1 :: Prefix1, Head2 :: Prefix2, (Head1, Head2) :: Result] =
    (cons1: Head1 :: Prefix1, cons2: Head2 :: Prefix2) => {
      val HCons(head1, prefix1) = cons1
      val HCons(head2, prefix2) = cons2
      HCons((head1, head2), zippable(prefix1, prefix2))
    }
}
