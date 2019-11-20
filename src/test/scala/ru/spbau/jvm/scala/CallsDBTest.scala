package ru.spbau.jvm.scala

import java.io.File
import java.time.LocalDateTime

import ru.spbau.jvm.scala.CallsDB.{Call, Name}

class CallsDBTest extends org.scalatest.FlatSpec {
  val db = CallsDB(new File(getClass.getResource("/").getFile))

  "calls" should "return all calls" in {
    assert(db.getCalls().length == 30)
  }

  it should "return calls in the specified period" in {
    val from = LocalDateTime.parse("2019-11-14T17:25:15")
    val to = LocalDateTime.parse("2019-11-16T12:18:00")
    assert(db.getCalls(from, to).length == 5)
  }

  it should "return right calls and include edge cases" in {
    val from = LocalDateTime.parse("2019-11-07T18:36:08")
    val to = LocalDateTime.parse("2019-11-08T13:38:12")
    val Seq(firstCall, secondCall) = db.getCalls(from, to)
    assert(firstCall == Call(Name("Boris", "Johnson"), "+441632960156", 141, 1.08F))
    assert(secondCall == Call(Name("Mark", "Rutte"), "+441632960159", 57, 0.44F))
  }

  it should "return no calls" in {
    val toTime = LocalDateTime.parse("2019-09-09T13:00:00")
    assert(db.getCalls(to = toTime).isEmpty)
  }

  "total" should "compute sum right" in {
    assert(math.abs(db.getTotal(LocalDateTime.parse("2019-11-10T23:46:09")) - 12.59) < 0.00001)
  }

  it should "return zero if no calls were made" in {
    val toTime = LocalDateTime.parse("2019-09-09T13:00:00")
    assert(db.getTotal(to = toTime) == 0)
  }

  "avg" should "compute average right" in {
    val from = LocalDateTime.parse("2019-11-05T17:45:01")
    val to = LocalDateTime.parse("2019-11-06T22:12:27")
    assert(db.getAvg(from, to) == 38)
  }

  it should "return zero if no calls were made" in {
    val fromTime = LocalDateTime.parse("2019-12-05T17:45:01")
    assert(db.getAvg(from = fromTime) == 0)
  }

  "number" should "return right number" in {
    assert(db.getNumber(Name("Leo", "Varadkar")) == "+441632960156")
  }

  it should "throw NoSuchElementException on wrong name" in {
    assertThrows[NoSuchElementException] {
      db.getNumber(Name("Wrong", "Name"))
    }
  }

  "employee" should "return right employee" in {
    assert(db.getEmployee("+441632960152") contains Name("Donald", "Tusk"))
  }

  it should "return None if number is not assigned" in {
    assert(db.getEmployee("+441632960172").isEmpty)
  }

  it should "throw NoSuchElementException if that number is not registered" in {
    assertThrows[NoSuchElementException] {
      db.getEmployee("+441632960192")
    }
  }

  "outgoing" should "return all calls made by the specified employee" in {
    val name = Name("Xavier", "Bettel")
    val Seq(first, second) = db.getOutgoing(name)
    assert(first == Call(name, "+441632960162", 283, 2.17F))
    assert(second == Call(name, "+441632960155", 289, 2.22F))
  }

  it should "return empty seq if the specified employee made no calls" in {
    val name = Name("Person", "WithoutCalls")
    assert(db.getOutgoing(name).isEmpty)
  }

  it should "throw NoSuchElementException if that employee does not exist" in {
    assertThrows[NoSuchElementException] {
      db.getOutgoing(Name("No", "Employee"))
    }
  }

  "incoming" should "return all calls received by the specified employee during the specified period" in {
    val from = LocalDateTime.parse("2019-11-05T07:10:00")
    val to = LocalDateTime.parse("2019-11-10T10:21:55")
    val name = Name("Antti", "Rinne")
    assert(db.getIncoming(name, from, to) == Seq(Call(Name("Mark", "Rutte"), "+441632960159", 57, 0.44F)))
  }

  it should "throw NoSuchElementException if that employee does not exist" in {
    assertThrows[NoSuchElementException] {
      db.getIncoming(Name("NoSuch", "Person"))
    }
  }
}
