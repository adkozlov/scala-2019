package ru.spbau.jvm.scala.lecture06

import org.scalatest._
import Main.zip
import Main.splitAt

class MainTest extends FlatSpec with Matchers {

  val testList1 = "a" :: "b" :: 1 :: 2 :: true :: false :: ("kek" :: "lol" :: HNil) :: "4eburek" :: HNil
  val testList2 = HNil
  val testList3 = 1 :: true :: "" :: testList1 :: testList2 :: HNil
  val testList4 = 3 :: testList3 :: true :: HNil

  "zip" should "compile" in {
    zip(testList1, testList1)
    zip(testList1, testList2)
    zip(testList1, testList3)
    zip(testList1, testList4)
    zip(testList2, testList1)
    zip(testList2, testList2)
    zip(testList2, testList3)
    zip(testList2, testList4)
    zip(testList3, testList1)
    zip(testList3, testList2)
    zip(testList3, testList3)
    zip(testList3, testList4)
    zip(testList4, testList1)
    zip(testList4, testList2)
    zip(testList4, testList3)
    zip(testList4, testList4)
  }

  it should "be correct" in {
    assert(zip("a" :: 1 :: HNil, 2 :: "b" :: HNil) == ("a", 2) :: (1, "b") :: HNil)
    assert(zip(true :: 1 :: HNil, 2 :: false :: HNil) == (true, 2) :: (1, false) :: HNil)
    assert(zip(true :: (1 :: 2 :: HNil) :: HNil, (2 :: 3 :: HNil) :: false :: HNil)
      == (true, (2 :: 3 :: HNil)) :: ((1 :: 2 :: HNil), false) :: HNil)
  }

  "splitAt" should "compile" in {
    splitAt(testList1, NilIndex)
    splitAt(testList1, AtLeastOneIndex(NilIndex))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(NilIndex)))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex)))))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(
      AtLeastOneIndex(NilIndex)))))))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(
      AtLeastOneIndex(AtLeastOneIndex(NilIndex))))))))
    splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(
      AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex)))))))))
    // compile error, too big index 
    //splitAt(testList1, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(
    //  AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))))))))

    splitAt(testList2, NilIndex)
    // compile error, too big index 
    //splitAt(testList2, AtLeastOneIndex(NilIndex))

    splitAt(testList3, NilIndex)
    splitAt(testList3, AtLeastOneIndex(NilIndex))
    splitAt(testList3, AtLeastOneIndex(AtLeastOneIndex(NilIndex)))
    splitAt(testList3, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))
    splitAt(testList3, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex)))))
    splitAt(testList3, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))))
    // compile error, too big index 
    //splitAt(testList3, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(
    //  AtLeastOneIndex(NilIndex)))))))


    splitAt(testList4, NilIndex)
    splitAt(testList4, AtLeastOneIndex(NilIndex))
    splitAt(testList4, AtLeastOneIndex(AtLeastOneIndex(NilIndex)))
    splitAt(testList4, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))
    // compile error, too big index 
    // splitAt(testList4, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex)))))
  }

  it should "be correct" in {
    assert(splitAt(1 :: true :: "a" :: HNil, NilIndex)
      == (HNil, 1 :: true :: "a" :: HNil))
    assert(splitAt(1 :: true :: "a" :: HNil, AtLeastOneIndex(NilIndex))
      == (1 :: HNil, true :: "a" :: HNil))
    assert(splitAt(1 :: true :: "a" :: HNil, AtLeastOneIndex(AtLeastOneIndex(NilIndex)))
      == (1 :: true :: HNil, "a" :: HNil))
    assert(splitAt(1 :: true :: "a" :: HNil, AtLeastOneIndex(AtLeastOneIndex(AtLeastOneIndex(NilIndex))))
      == (1 :: true :: "a" :: HNil, HNil))
  }
}