package ru.spbau.jvm.scala.heterogeneous

import org.scalatest.FlatSpec

class TestHeterogeneous extends FlatSpec {
  "Zip function " should " return list of homogenous pairs " in {
    val lst1 = 5 :: "Hello" :: false :: HNil
    val lst2 = 7 :: "World" :: true :: HNil
    val zipped = lst1.zip(lst2)
    val int1: Int = zipped.head._1
    val int2: Int = zipped.head._2
    val str1: String = zipped.tail.head._1
    val str2: String = zipped.tail.head._2
    val bool1: Boolean = zipped.tail.tail.head._1
    val bool2: Boolean = zipped.tail.tail.head._2
    val nil1: Nil = zipped.tail.tail.tail
    val nil2: Nil = zipped.tail.tail.tail
    assert(int1 === 5)
    assert(int2 === 7)
    assert(str1 === "Hello")
    assert(str2 === "World")
    assert(bool1 === false)
    assert(bool2 === true)
    assert(nil1 === HNil)
    assert(nil2 === HNil)
  }

  "Zip function " should " return list of two pairs in" in {
    val lst1 = 5 :: "abracadabra" :: "something" :: HNil
    val lst2 = "hey" :: 7 :: HNil
    val zipped = lst1.zip(lst2)
    val int1 = zipped.head._1
    val str2 = zipped.head._2
    val str1 = zipped.tail.head._1
    val int2 = zipped.tail.head._2
    val bottom: Nil = zipped.tail.tail
    assert(int1 === 5)
    assert(str2 === "hey")
    assert(str1 === "abracadabra")
    assert(int2 === 7)
    assert(bottom === HNil)
  }

  def testFullList(lst: HCons[String, HCons[Int, HCons[Boolean, HNil.type]]]): Unit = {
    assert("hello" === lst.head)
    assert(42 === lst.tail.head)
    assert(false === lst.tail.tail.head)
    assert(HNil === lst.tail.tail.tail)
  }

  "Split on Zero " should " return Nil in first elem" in {
    val lst = "hello" :: 42 :: false :: HNil
    val pair = lst.splitAt(Zero)
    assert(HNil === pair._1)
    testFullList(pair._2)
  }

  "Split on Last " should " return Nil in second elem" in {
    val lst = "hello" :: 42 :: false :: HNil
    val pair = lst.splitAt(Succ(Succ(Succ(Zero))))
    assert(HNil === pair._2)
    testFullList(pair._1)
  }

  "Split on middle element " should "return not Nil lists" in {
    val lst = "hello" :: 42 :: false :: HNil
    val pair = lst.splitAt(Succ(Zero))
    assert(pair._1.head === "hello")
    assert(pair._2.head === 42)
    assert(pair._2.tail.head === false)
    assert(pair._2.tail.tail === HNil)
  }




}
