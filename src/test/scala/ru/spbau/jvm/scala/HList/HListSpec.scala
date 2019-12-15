package ru.spbau.jvm.scala
package HList
import lecture06._
import org.scalatest.matchers.should.Matchers
import ru.spbau.jvm.scala.lecture06

class HListSpec extends org.scalatest.FlatSpec with Matchers {
  "zip" should "zip two equal size lists" in {
    val a = 2 :: 6 :: "S" :: HNil
    val b = "S" :: 8 :: "42" :: HNil
    a zip b should be ((2, "S") :: (6, 8) :: ("S", "42") :: HNil)
  }

  "zip" should "zip two different size lists" in {
    val a = 2 :: 6 :: 239 :: 42 :: 53 :: HNil
    val b = "S" :: 8 :: "42" :: HNil
    a zip b should be ((2, "S") :: (6, 8) :: (239, "42") :: HNil)
  }

  "zip" should "zip two nills" in {
    HNil zip HNil should be (HNil)
  }

  "zip list with emplty list" should "be empty list" in {
    val b = 2 :: "e" :: 4 :: HNil
    HNil zip b should be (HNil)
    b zip HNil should be (HNil)
  }

  "split" should "work on simple samples" in {
    val a = 2 :: "e" :: 4 :: HNil
    a splitAt Inc(Zero) should be (2 :: HNil, "e" :: 4 :: HNil)
  }

  "splitAt 0" should "return empty list and full list" in {
    val a = 2 :: "e" :: 4 :: HNil
    a splitAt Zero should be (HNil, a)
  }

  "splitAt 0" should "work on empty list" in {
    HNil splitAt Zero should be (HNil, HNil)
  }

  "splitAt list size" should "work" in {
    val a = 2 :: "e" :: 4 :: HNil
    a splitAt Inc(Inc(Inc(Zero))) should be (a, HNil)
  }

  "splitAt greater list size" should "not compile" in {
    val a = 2 :: "e" :: 4 :: HNil
    assertDoesNotCompile("a splitAt Inc(Inc(Inc(Inc(Zero))))")
  }
}
