package ru.spbau.jvm.scala.hlist

import org.scalatest._

class HListTest extends FlatSpec {

  import HListTest._

  behavior of "zip"

  it should "work correctly with Nils" in {
    assert((HNil zip HNil) == HNil)
    assert((shortList zip HNil) == HNil)
    assert((HNil zip longList) == HNil)
  }

  it should "keep the order of elements" in {
    val expected = (1, true) :: ("two", "false") :: HNil
    assert((shortList zip anotherShortList) == expected)
  }

  it should "be the length of first if it is shorter" in {
    val expected = (1, "one") :: ("two", 2) :: HNil
    assert((shortList zip longList) == expected)
  }

  it should "be the length of second if it is shorter" in {
    val expected = ("one", 1) :: (2, "two") :: HNil
    assert((longList zip shortList) == expected)
  }

  it can "be applied multiple times" in {
    val expected = ((1, true), "one") :: (("two", "false"), 2) :: HNil
    assert((shortList zip anotherShortList zip longList) == expected)
  }

  behavior of "splitAt"

  it should "work correctly with Nil and Zero index" in {
    val (left, right) = HNil.splitAt(Zero)
    assert(left == HNil)
    assert(right == HNil)
  }

  it should "work correctly with Zero index" in {
    val (left, right) = longList.splitAt(Zero)
    assert(left == HNil)
    assert(right == longList)
  }

  it should "split at the right place" in {
    {
      val (left, right) = longList.splitAt(one)
      assert(left == "one" :: HNil)
      assert(right == 2 :: ("three" :: HNil) :: 4.0 :: "5" :: HNil)
    }
    {
      val (left, right) = longList.splitAt(two)
      assert(left == "one" :: 2 :: HNil)
      assert(right == ("three" :: HNil) :: 4.0 :: "5" :: HNil)
    }
    {
      val (left, right) = longList.splitAt(three)
      assert(left == "one" :: 2 :: ("three" :: HNil) :: HNil)
      assert(right == 4.0 :: "5" :: HNil)
    }
    {
      val (left, right) = longList.splitAt(four)
      assert(left == "one" :: 2 :: ("three" :: HNil) :: 4.0 :: HNil)
      assert(right == "5" :: HNil)
    }
  }

  it should "split the end of list correctly" in {
    {
      val (left, right) = shortList.splitAt(two)
      assert(left == shortList)
      assert(right == HNil)
    }
    {
      val (left, right) = longList.splitAt(five)
      assert(left == longList)
      assert(right == HNil)
    }
  }
}

object HListTest {
  private val shortList = 1 :: "two" :: HNil
  private val anotherShortList = true :: "false" :: HNil
  private val longList = "one" :: 2 :: ("three" :: HNil) :: 4.0 :: "5" :: HNil
  private val one = Succ(Zero)
  private val two = Succ(one)
  private val three = Succ(two)
  private val four = Succ(three)
  private val five = Succ(four)
}
