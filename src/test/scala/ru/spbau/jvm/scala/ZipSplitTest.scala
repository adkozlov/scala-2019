package ru.spbau.jvm.scala

import org.scalatest.matchers.should.Matchers
import ru.spbau.jvm.scala.ListUtils._

class ZipSplitTest extends org.scalatest.FlatSpec with Matchers {

  private val list = "hello" :: 42 :: false :: 'a' :: HNil
  private val firstList = "hello" :: 42 :: HNil
  private val secondList = false :: 'a' :: HNil

  it should "split correctly in the middle" in {
    val (left, right) = list.split(Number(Number(Nil)))
    assert(left === "hello" :: 42 :: HNil)
    assert(right === false :: 'a' :: HNil)
  }

  it should "split empty list on zero" in {
    val (left, right) = HNil.split(Nil)
    assert(left === HNil)
    assert(right === HNil)
  }

  it should "split list at the end" in {
    val (left, right) = list.split(Number(Number(Number(Number(Nil)))))
    assert(left === list)
    assert(right === HNil)
  }

  it should "split list at the beginning" in {
    val (left, right) = list.split(Nil)
    assert(left === HNil)
    assert(right === list)
  }

  it should "zip lists with equal length" in {
    val result = firstList.zip(secondList)
    assert(result === ("hello", false) :: (42, 'a') :: HNil)
  }

  it should "zip lists with different length" in {
    val result = list.zip(secondList)
    assert(result === ("hello", false) :: (42, 'a') :: HNil)
  }

  it should "zip with empty" in {
    val result = list.zip(HNil)
    assert(result === HNil)
  }

  it should "zip empty with not empty" in {
    val result = HNil.zip(list)
    assert(result === HNil)
  }

  it should "zip two empty" in {
    val result = HNil.zip(HNil)
    assert(result === HNil)
  }
}
