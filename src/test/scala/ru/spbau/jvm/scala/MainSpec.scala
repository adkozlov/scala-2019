package ru.spbau.jvm.scala

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

class MainSpec extends FlatSpec with Matchers {
  Main.database
    .loadDatabase("resources/testusers.csv", "resources/testcalls.csv")

  "getCallsByPeriod" should "return every call from maximum period" in {
    Main.getCallsByPeriod(
      new DateTime(Long.MinValue),
      new DateTime(Long.MaxValue)
    ) should be(
      List(
        Call(
          Number("a", "+1 234 56 78"),
          "+2 345 67 89",
          DateTime.parse("01.09.2019", Database.formatter),
          60,
          "1.1".toDouble
        ),
        Call(
          Number("b", "+2 345 67 89"),
          "+3 456 78 90",
          DateTime.parse("02.09.2019", Database.formatter),
          120,
          "3.2".toDouble
        ),
        Call(
          Number("b", "+2 345 67 89"),
          "+4 567 89 01",
          DateTime.parse("02.09.2019", Database.formatter),
          120,
          "3.3".toDouble
        ),
        Call(
          Number("e", "+7 999 99 99"),
          "+328 998 22 88",
          DateTime.parse("03.09.2019", Database.formatter),
          10,
          "10".toDouble
        )
      )
    )
  }

  "getCallsFromName" should "return every call by Name" in {
    Main.getCallsFromName("b") should be(
      List(
        Call(
          Number("b", "+2 345 67 89"),
          "+3 456 78 90",
          DateTime.parse("02.09.2019", Database.formatter),
          120,
          "3.2".toDouble
        ),
        Call(
          Number("b", "+2 345 67 89"),
          "+4 567 89 01",
          DateTime.parse("02.09.2019", Database.formatter),
          120,
          "3.3".toDouble
        )
      )
    )
  }

  it should "return nothing by bad name" in {
    Main.getCallsFromName("undefined nonexistent name") should be(List())
  }

  "getCallsToNumber" should "return every call by Number" in {
    Main.getCallsToNumber("+328 998 22 88") should be(
      List(
        Call(
          Number("e", "+7 999 99 99"),
          "+328 998 22 88",
          DateTime.parse("03.09.2019", Database.formatter),
          10,
          "10".toDouble
        )
      )
    )
  }

  it should "return nothing by bad number" in {
    Main.getCallsToNumber("+228 1337 1488") should be(List())
  }

  "getTotalByPeriod" should "return correct total by all time" in {
    Main.getTotalByPeriod(
      new DateTime(Long.MinValue),
      new DateTime(Long.MaxValue)
    ) should be(17.6)
  }

  "avg" should "be correct" in {
    Main.avg() should be(77.5)
  }

  "getNumberFromName" should "be correct" in {
    Main.getNumberFromName("a") should be("+1 234 56 78")
    Main.getNumberFromName("c") should be("+3 456 78 90")
  }

  it should "be \"There is no such man\" for bad name" in {
    Main.getNumberFromName("bad name") should be("There is no such man")
  }

  "getTotalFromName" should "be correct" in {
    Main.getTotalFromName("e") should be(10)
    Main.getTotalFromName("b") should be(6.5)
  }

  it should "be zero for bad name" in {
    Main.getTotalFromName("bad name") should be(0)
  }
}
