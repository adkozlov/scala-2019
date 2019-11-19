package ru.spbau.jvm.scala

import java.text.SimpleDateFormat

import org.scalatest.FlatSpec

class DatabaseTest extends FlatSpec {
  Database.load()

  "Average duration" should "produce a proper value" in {
    assert((Database.getAverageDuration - 44).abs < 0.001)
  }

  "Max spender" should "produce a proper value" in {
    val maxSpender = Database.getMaxSpender
    assert(maxSpender.firstName == "Petr" && maxSpender.lastName == "Petrov")
  }

  "Query for calls for the given period" should "produce a proper value" in {
    assert(Database.getCallsBetweenDates(new SimpleDateFormat("dd.MM.yyyy").parse("01.09.2019"),
      new SimpleDateFormat("dd.MM.yyyy").parse("01.11.2019")).size == 2)
  }

  "Query for total costs of the calls for the given period" should "produce a proper value" in {
    assert(Database.getTotalBetweenDates(new SimpleDateFormat("dd.MM.yyyy").parse("01.01.2019"),
      new SimpleDateFormat("dd.MM.yyyy").parse("01.04.2019")) == 3)
  }

  "Get number by the name" should "return a proper number if user with this name exists" in {
    assert(Database.getNumberByName("Ivan", "Ivanov").get == "+7-900-123-12-00")
  }

  "Get number by the name" should "return None if user with this name does not exists" in {
    assert(Database.getNumberByName("Ivan", "Petrov").isEmpty)
  }

  "Query for calls with the cost more than a given parameter" should "produce a proper value" in {
    assert(Database.getCallsWithCostMoreOrEqualTo(2).size == 1)
  }

  "Query for calls with the duration more than a given parameter" should "produce a proper value" in {
    assert(Database.getCallsWithDurationMoreOrEqualTo(50).size == 3)
  }
}
