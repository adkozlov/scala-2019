package ru.spbau.jvm.scala.heterogeneous

import org.scalatest.FunSuite
import ru.spbau.jvm.scala.lecture06._

class ZipTest extends FunSuite {
  private val nonEmptyList = 1 :: 2 :: "kek" :: HNil

  test("zip non-empty with HNil") {
    assert(HNil == nonEmptyList.zip(HNil))
  }

  test("zip HNil with non-empty") {
    assert(HNil == HNil.zip(nonEmptyList))
  }

  test("zip equal sized lists") {
    val firstList = 1 :: 2 :: HNil
    val secondList = "1" :: "2" :: HNil
    val expected = (1, "1") :: (2, "2") :: HNil
    assertCompare(expected, firstList.zip(secondList))
  }

  test("zip first shorted") {
    val firstList = 1 :: 2 :: HNil
    val secondList = "1" :: "2" :: "3" :: HNil
    val expected = (1, "1") :: (2, "2") :: HNil
    assertCompare(expected, firstList.zip(secondList))
  }

  test("zip second shorted") {
    val firstList = 1 :: 2 :: 3 :: HNil
    val secondList = "1" :: "2" :: HNil
    val expected = (1, "1") :: (2, "2") :: HNil
    assertCompare(expected, firstList.zip(secondList))
  }

  private def assertCompare(left: HList, right : HList): Unit = {
    if (left == HNil || right == HNil)
      assert(left == right)
    else {
      val HCons(lhead, ltail) = left
      val HCons(rhead, rtail) = right
      assert(lhead == rhead)
      assertCompare(ltail, rtail)
    }
  }
}
