package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class HListTest extends FlatSpec with Matchers {
  private val notNilList1 = true :: "aaaa" :: 228 :: HNil
  private val notNilList2 = "ppppp" :: 1 :: notNilList1 :: false :: 42 :: HNil

  "zip" should "work correctly when both lists are HNil" in {
    HNil.zip(HNil) should be (HNil)
  }

  "zip" should "work correctly when one the lists is HNil" in {
    HNil.zip(notNilList1) should be (HNil)
    notNilList1.zip(HNil) should be (HNil)
  }

  "zip" should "work correctly when both lists are HCons" in {
    notNilList1.zip(notNilList2) should be ((true, "ppppp") :: ("aaaa", 1) :: (228, notNilList1) :: HNil)
    notNilList2.zip(notNilList1) should be (("ppppp", true) :: (1, "aaaa") :: (notNilList1, 228) :: HNil)
  }

  "splitAt" should "work correctly when index is zero" in {
    notNilList2.splitAt(Zero) should be (HNil, notNilList2)
  }

  "splitAt" should "work correctly when index is the size of list" in {
    notNilList2.splitAt(Suc(Suc(Suc(Suc(Suc(Zero)))))) should be (notNilList2, HNil)
  }

  "splitAt" should "work correctly when index is somewhere in the middle in the list" in {
    notNilList2.splitAt(Suc(Suc(Zero))) should be ("ppppp" :: 1 :: HNil, notNilList1 :: false :: 42 :: HNil)
  }
}