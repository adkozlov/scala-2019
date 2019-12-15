package ru.spbau.jvm.scala.heterogeneous

import org.scalatest._
import ru.spbau.jvm.scala.heterogeneous.HList.HNil
import ru.spbau.jvm.scala.heterogeneous.NonNegative.{Next, Zero}

class HListSpec extends FlatSpec with Matchers {

  private val list1 = "kek" :: 1 :: false :: HNil
  private val list2 = 42 :: "6x7" :: HNil

  private val one = Next(Zero)
  private val two = Next(one)
  private val three = Next(two)

  "Zip" should "work correctly" in {
    HNil.zip(HNil)   shouldBe HNil
    HNil.zip(list1)  shouldBe HNil
    list1.zip(HNil)  shouldBe HNil
    list1.zip(list2) shouldBe ("kek", 42) :: (1, "6x7") :: HNil
    list2.zip(list1) shouldBe (42, "kek") :: ("6x7", 1) :: HNil
  }

  "SplitAt" should "work correctly" in {
    HNil.splitAt(Zero)    shouldBe (HNil, HNil)
    list1.splitAt(Zero)   shouldBe (HNil, list1)
    list1.splitAt(one)    shouldBe ("kek" :: HNil, 1 :: false :: HNil)
    list1.splitAt(three)  shouldBe (list1, HNil)
  }

  "SplitAt incorrect length" should "not compile" in {
    assertDoesNotCompile("list2.splitAt(three)")
  }
}
