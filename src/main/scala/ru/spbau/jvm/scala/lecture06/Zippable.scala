package ru.spbau.jvm.scala
package lecture06

trait Zippable[First <: HList, Second <: HList, Result <: HList] {
  def apply(first: First, second: Second): Result
}

object Zippable {

  implicit def firstNilZippable[Second <: HList]: Zippable[Nil, Second, Nil] =
    (_: Nil, _: Second) => HNil

  implicit def secondNilZippable[First <: HList]: Zippable[First, Nil, Nil] =
    (_: First, _: Nil) => HNil


  implicit def firstSecondNilZippable: Zippable[Nil, Nil, Nil] =
    (_: Nil, _: Nil) => HNil

  implicit def zippable[Head1,
                        Head2,
                        First <: HList,
                        Second <: HList,
                        Result <: HList](
    implicit zippable: Zippable[First, Second, Result]
  ): Zippable[Head1 :: First, Head2 :: Second, (Head1, Head2) :: Result] =
    (first: Head1 :: First, second: Head2 :: Second) => {
      val HCons(head1, list1) = first
      val HCons(head2, list2) = second
      HCons((head1, head2), zippable(list1, list2))
    }
}
