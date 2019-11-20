package ru.spbau.jvm.scala

import org.scalatest.FlatSpec
import java.time.LocalDate
import java.time.format.DateTimeFormatter

class TableTest extends FlatSpec{
  val employees = Array(new Employee("Ivan", "Ivanov", "+7976531"),
                        new Employee("A", "B", "+7921321"),
                        new Employee("Makar", "Selivanov", "+792843231"),
                        new Employee("Misha", "Mishin", "8932453"))
  val calls = Array(
    Call.parseFromString("+792843231|10.03.2017|1|0;"),
    Call.parseFromString("+792843231|11.04.2007|200|20;"),
    Call.parseFromString("8932453|10.10.3010|30|130;"),
    Call.parseFromString("8932453|01.12.1010|30|130;"),
    Call.parseFromString("+792843231|10.03.2017|1|0;"),
    Call.parseFromString("+7976531|10.03.2017|1|0;"),
    Call.parseFromString("8932453|10.10.2010|30|130;")
  )
  val table = new Table(employees, calls)

  private def toLocalTime(date: String) = {
    val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    LocalDate.parse(date, formatter)
  }

  "List for nonexistent employee" should " returns null" in {
    assert(table.callsByEmployee("C", "D") == null)
  }

  "List for employee without call" should " returns empty array" in {
    assert(table.callsByEmployee("A", "B").isEmpty)
  }

  "List for employee with calls" should " returns all his calls" in {
    assert(table.callsByEmployee("Ivan", "Ivanov") sameElements Array(calls(5)))
  }

  "Avg without time period " should "returns average of all calls" in {
    val durations = calls.map(_.duration)
    assert(table.avg(null, null) == durations.sum / durations.length)
  }

  "Avg with upper bound " should " returns average of all calls before it" in {
    assert(table.avg(null, toLocalTime("01.12.1010")) == 30)
  }

  "Avg with lower bound " should " returns average of all calls after it" in {
    assert(table.avg(toLocalTime("10.10.3010"), null) == 30)
  }

  "Avg with both bounds " should " returns average of calls in the period" in {
    assert(table.avg(toLocalTime("01.12.1010"), toLocalTime("11.04.2007")) == 115)
  }

  "Count without time period " should "returns count of all calls" in {
    assert(table.count(null, null) == calls.length)
  }

  "Count with upper bound " should " returns count of all calls before it" in {
    assert(table.count(null, toLocalTime("01.12.1010")) == 1)
  }

  "Count with lower bound " should " returns count of all calls after it" in {
    assert(table.count(toLocalTime("10.10.3010"), null) == 1)
  }

  "Count with both bounds " should " returns count of calls in the period" in {
    assert(table.count(toLocalTime("01.12.1010"), toLocalTime("11.04.2007")) == 2)
  }

  "Total without time period " should "returns total cost of all calls" in {
    assert(table.total(null, null) == calls.map(_.cost).sum)
  }

  "Total with upper bound " should " returns total cost of all calls before it" in {
    assert(table.total(null, toLocalTime("01.12.1010")) == 130)
  }

  "Total with lower bound " should " returns total cost of all calls after it" in {
    assert(table.total(toLocalTime("10.10.3010"), null) == 130)
  }

  "Total with both bounds " should " returns total cost of calls in the period" in {
    assert(table.total(toLocalTime("01.12.1010"), toLocalTime("11.04.2007")) == 150)
  }

  "Number for existing employee " should " returns his number" in {
    assert(table.number("Makar", "Selivanov") == "+792843231")
  }

  "Number for nonexistent employee " should " returns null" in {
    assert(table.number("Makar", "Nikolukin") == null)
  }

  "Employee for existing number " should " returns relative Employee object" in {
    assert(table.numberHolder(employees(0).number) == employees(0))
  }

  "Employee for nonexistent number " should " returns null" in {
    assert(table.numberHolder(employees(0).number + "10") == null)
  }

  "Calls with upper bound " should " returns all calls before it" in {
    assert(table.calls(null, toLocalTime("01.12.1010")) sameElements Array((employees(3), calls(3))))
  }

  "Calls with lower bound " should " returns all calls after it" in {
    assert(table.calls(toLocalTime("10.10.3010"), null) sameElements Array((employees(3), calls(2))))
  }

  "Calls with both bounds " should " returns calls in the period" in {
    assert(table.calls(toLocalTime("01.12.1010"), toLocalTime("11.04.2007")).sortBy(_._2.date) sameElements Array((employees(3), calls(3)), (employees(2), calls(1))).sortBy(_._2.date))
  }
}
