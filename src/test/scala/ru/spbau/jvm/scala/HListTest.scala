package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class HListTest extends FlatSpec with Matchers {
  private val smallList = "Deutsch" ::: 1412 ::: HList.HNil
  private val bigList = true ::: "Germany" ::: 7 ::: (12.19 ::: true ::: HList.HNil) ::: false ::: "Lang" ::: HList.HNil

  "zipNilNil" should "ok" in {
    HList.HNil.zip(HList.HNil) should be(HList.HNil)
  }

  "zipConsNil" should "ok" in {
    smallList.zip(HList.HNil) should be(HList.HNil)
  }

  "zipNilCons" should "ok" in {
    HList.HNil.zip(smallList) should be(HList.HNil)
  }

  "zipConsCons" should "ok" in {
    smallList.zip(bigList) should be(
      ("Deutsch", true) ::: (1412, "Germany") ::: HList.HNil
    )
    bigList.zip(smallList) should be(
      (true, "Deutsch") ::: ("Germany", 1412) ::: HList.HNil
    )
  }

  "splitAtNil" should "ok" in {
    HList.HNil.splitAt(Peano.Z) should be(HList.HNil, HList.HNil)
  }

  "splitAtConsWithZeroIdx" should "ok" in {
    smallList.splitAt(Peano.Z) should be(smallList, HList.HNil)
  }

  "splitAtCons" should "ok" in {
    smallList.splitAt(Peano.Z) should be(smallList, HList.HNil)
    smallList.splitAt(Peano.S(Peano.Z)) should be(
      1412 ::: HList.HNil,
      "Deutsch" ::: HList.HNil
    )
    smallList.splitAt(Peano.S(Peano.S(Peano.Z))) should be(
      HList.HNil,
      smallList
    )
  }
}
