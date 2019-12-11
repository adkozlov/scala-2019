package ru.spbau.jvm.scala.lecture06

trait Zippable[
  Prefix <: HList,
  List <: HList,
  Result <: HList
] {
  def apply(prefix: Prefix, list: List): Result
}

object Zippable {
  // Nil zip List = Nil
  implicit def leftNilZippable[
    List <: HList
  ]: Zippable[Nil, List, Nil] =
    (_: Nil, _: List) => HNil

  // Cons zip Nil = Nil
  implicit def rightNilZippable[
    Head,
    Tail <: HList
  ]: Zippable[Head :: Tail, Nil, Nil] =
    (_: Head :: Tail, _: Nil) => HNil

  // Prefix zip List = Result
  // (Head1 :: Prefix) zip (Head2 :: List) = (Head1, Head2) :: Result
  implicit def zippable[
    Head1,
    Head2,
    Prefix <: HList,
    List <: HList,
    Result <: HList
  ](implicit zippable: Zippable[Prefix, List, Result]): Zippable[Head1 :: Prefix, Head2 :: List, (Head1, Head2) :: Result] =
    (cons1: Head1 :: Prefix, cons2: Head2 :: List) => {
      val HCons(head1, prefix) = cons1
      val HCons(head2, list) = cons2
      HCons((head1, head2), zippable(prefix, list))
    }
}
