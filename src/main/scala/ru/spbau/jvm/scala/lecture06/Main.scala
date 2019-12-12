package ru.spbau.jvm.scala
package lecture06

object Main {

  import Splittable._
  import Zippable._
  import Appendable._

  def zip[First <: HList, Second <: HList, Result <: HList](
    first: First,
    second: Second
  )(implicit zippable: Zippable[First, Second, Result]): Result =
    zippable.apply(first, second)

  def splitAt[List <: HList, Ind <: Index, Result <: (HList, HList)](
    list: List,
    index: Ind
  )(implicit splittable: Splittable[List, Ind, Result]): Result =
    splittable.apply(list, index)

  def main(args: Array[String]): Unit = {
    val helloPrefix = "hello" :: 42 :: false :: HNil
    val worldSuffix = "world" :: HNil

    zip(helloPrefix, worldSuffix)
    splitAt(worldSuffix, AtLeastOneIndex(NilIndex))

    val testList = "a" :: "b" :: 1 :: 2 :: true :: false ::
      ("kek" :: "lol" :: HNil) :: "4eburek" :: HNil
    zip(testList, testList)
    zip(testList, helloPrefix)
    zip(testList, worldSuffix)


  }
}
