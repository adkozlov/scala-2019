package ru.spbau.jvm.scala.hlist

import org.scalatest.{FlatSpec, Matchers}

class HListZipTests extends FlatSpec with Matchers {
  private def testList1() = 1 :: 2 :: 3 :: true :: HNil

  private def testList2() = true :: false :: true :: "15" :: HNil

  private def testList3() = true :: false :: HNil

  "Zip on testList1 and testList2" should "work correctly" in {
    testList1().zip(testList2()) should be ((1, true) :: (2, false) :: (3, true) :: (true, "15") :: HNil)
  }

  "Zip on testList2 and testList1" should "give reversed order" in {
    testList2().zip(testList1()) should be ((true, 1) :: (false, 2) :: (true, 3) :: ("15", true) :: HNil)
  }

  "Zip with different sized lists" should "has minimum size" in {
    testList1().zip(testList3()) should be ((1, true) :: (2, false) :: HNil)
  }
}
