package ru.spbau.jvm.scala
package lecture06

import org.scalatest.FlatSpec
import ru.spbau.jvm.scala.lecture06.Splittable._
import ru.spbau.jvm.scala.lecture06.Zippable._

class HListSpec extends FlatSpec {

  import HListSpec._

  "Nil zip List" should "return Nil" in {
    assert((HNil zip list2) == HNil)
  }

  "List zip Nil" should "return Nil" in {
    assert((list1 zip HNil) == HNil)
  }

  "List1 zip List2" should "return zip" in {
    assert((list1 zip list2) == (1, 3) :: (2, 4) :: HNil)
  }

  "List2 zip List1" should "return zip" in {
    assert((list2 zip list1) == (3, 1) :: (4, 2) :: HNil)
  }

  "List2 zip List3" should "return zip" in {
    assert((list2 zip list3) == (3, 5) :: (4, 6) :: HNil)
  }

  "Nil splitAt" should "return Nil" in {
    assert(HNil.splitAt(Nat0) == (HNil, HNil))
  }

  "List splitAt 0" should "return this list" in {
    assert(list1.splitAt(Nat0) == (HNil, list1))
  }

  "List splitAt" should "split list" in {
    assert(list1.splitAt(Succ(Nat0)) == (1 :: HNil, 2 :: 3 :: HNil))
    assert(list1.splitAt(Succ(Succ(Nat0))) == (1 :: 2 :: HNil, 3 :: HNil))
    assert(list1.splitAt(Succ(Succ(Succ(Nat0)))) == (1 :: 2 :: 3 :: HNil, HNil))
  }
}

object HListSpec {
  private val list1 = 1 :: 2 :: 3 :: HNil
  private val list2 = 3 :: 4 :: HNil
  private val list3 = 5 :: 6 :: HNil
}
