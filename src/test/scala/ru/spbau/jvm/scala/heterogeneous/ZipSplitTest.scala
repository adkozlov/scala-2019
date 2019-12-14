package ru.spbau.jvm.scala.heterogeneous

import org.scalatest.FunSuite
import ru.spbau.jvm.scala.lecture06._

class ZipSplitTest extends FunSuite {
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

  test("empty list splits") {
    assert((HNil, HNil) == HNil.splitAt(NZero))
  }

  test("splits list in the middle") {
    val (start, end) = nonEmptyList.splitAt(Next(Next(NZero)))
    assertCompare(1 :: 2 :: HNil, start)
    assertCompare("kek" :: HNil, end)
  }

  test("splits list in the end") {
    val (start, end) = nonEmptyList.splitAt(Next(Next(Next(NZero))))
    assertCompare(nonEmptyList, start)
    assertCompare(HNil, end)
  }

  test("splits list at the start") {
    val (start, end) = nonEmptyList.splitAt(NZero)
    assertCompare(HNil, start)
    assertCompare(nonEmptyList, end)
  }

  // this test produces a compilation error
//  test("split on an index > list size does not compile") {
//    nonEmptyList.splitAt(Next(Next(Next(Next(NZero)))))
//  }

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

object Runner extends App {
  org.scalatest.run(new ZipSplitTest())
}
