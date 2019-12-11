package ru.spbau.jvm.scala
package lecture06

import org.scalatest.FunSuite

class ZipSuite extends FunSuite {

  test("Empty zip.") {
    val zip = HNil zip HNil
    assert(zip == HNil)
  }

  test("Equal length zip.") {
    val list1 = 1 :: "hey" :: HNil
    val list2 = "a" :: "x" :: HNil
    val zip = list1 zip list2
    assert(zip == (1, "a") :: ("hey", "x") :: HNil)
  }

  test("Longer left zip.") {
    val list1 = 1 :: "hey" :: 244 :: HNil
    val list2 = "a" :: "x" :: HNil
    val zip = list1 zip list2
    assert(zip == (1, "a") :: ("hey", "x") :: HNil)
  }

  test("Longer right zip.") {
    val list1 = 1 :: "hey" :: HNil
    val list2 = "a" :: "x" :: "hey" :: HNil
    val zip = list1 zip list2
    assert(zip == (1, "a") :: ("hey", "x") :: HNil)
  }
}
