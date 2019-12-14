package ru.spbau.jvm.scala.lecture06

import org.scalatest._

class HListTest extends FlatSpec {

  import Zippable._
  import Splittable._
  import HListTest._
  import Double.MaxValue

  "zip" should "return Nil when both are Nils" in {
    assert((HNil zip HNil) == HNil)
  }

  it should "return Nil when left is Nil" in {
    assert((HNil zip FirstList) == HNil)
  }

  it should "return Nil when right is Nil" in {
    assert((FirstList zip HNil) == HNil)
  }

  it should "ignore remaining elements of the left list (when left is bigger)" in {
    val zipped = ("abacaba", 12) :: (12, MaxValue) :: (true, "hi") :: ("hello", List()) :: HNil
    assert((FirstList zip SecondList) == zipped)
  }

  it should "ignore remaining elements of the right collection (when right is bigger)" in {
    val zipped = (12, "abacaba") :: (MaxValue, 12) :: ("hi", true) :: (List(), "hello") :: HNil
    assert((SecondList zip FirstList) == zipped)
  }

  it should "correctly zip equal sized lists" in {
    val zipped = (12, 12) :: (MaxValue, MaxValue) :: ("hi", "hi") :: (List(), List()) :: (-1, -1) :: HNil
    assert((SecondList zip SecondList) == zipped)
  }

  "splitAt" should "split Nil at Zero" in {
    assert(HNil.splitAt(Zero) == (HNil, HNil))
  }

  it should "split non-empty list at Zero" in {
    assert(FirstList.splitAt(Zero) == (HNil, FirstList))
  }

  it should "split list in the middle" in {
    val firstExpected = 12 :: Double.MaxValue :: HNil
    val secondExpected =  "hi" :: List() :: -1 :: HNil
    assert(SecondList.splitAt(Two) == (firstExpected, secondExpected))
  }

  it should "split list at the end" in {
    assert(FirstList.splitAt(Four) == (FirstList, HNil))
  }
}

object HListTest {
  private val FirstList = "abacaba" :: 12 :: true :: "hello" :: HNil
  private val SecondList = 12 :: Double.MaxValue :: "hi" :: List() :: -1 :: HNil
  private val Two = Succ(Succ(Zero))
  private val Four = Succ(Succ(Two))
}
