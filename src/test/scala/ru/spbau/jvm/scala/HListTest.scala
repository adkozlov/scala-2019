package ru.spbau.jvm.scala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Utils._

class HListTest extends AnyFlatSpec with Matchers {
    private val emptyList = HNil
    private val oneElemList = 1.0 :: HNil
    private val twoElemList = 10 :: "string" :: HNil
    private val one = Nat(Zero)

    it should "split with zero" in {
        val (l, r) = twoElemList.splitAt(Zero)
        assert(l === HNil)
        assert(r === twoElemList)
    }

    it should "split with one" in {
        val (l, r) = twoElemList.splitAt(one)
        assert(l === 10 :: HNil)
        assert(r === "string" :: HNil)
    }

    it should "zip" in {
        val z = twoElemList.zip(oneElemList)
        assert(z === (10, 1.0) :: HNil)
    }
}
