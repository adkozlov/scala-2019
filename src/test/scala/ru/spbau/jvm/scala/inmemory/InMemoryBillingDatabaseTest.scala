package ru.spbau.jvm.scala.inmemory

import java.time.LocalDateTime

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala._

class InMemoryBillingDatabaseTest extends FlatSpec with Matchers {
  val db: InMemoryBillingDatabase = new InMemoryBillingDatabase(
    users = Map(
      0 -> User("Alex", "Kane"),
      1 -> User("Alex", "Bane")
    ),
    actions = Map(
      0 -> Action(phoneId = 0, operationId = 1, LocalDateTime.parse("2019-11-20T11:59:59"), count = 1),
      1 -> Action(phoneId = 1, operationId = 0, LocalDateTime.parse("2019-11-20T10:59:59"), count = 2),
      2 -> Action(phoneId = 1, operationId = 2, LocalDateTime.parse("2019-11-20T10:55:59"), count = 3)
    ),
    phones = Map(
      0 -> Phone("12345"),
      1 -> Phone("54321")
    ),
    userPhones = Set(
      (0, 1),
      (1, 0)
    ),
    operations = Map(
      0 -> Operation("call in", 0.1f),
      1 -> Operation("call out", 1f),
      2 -> Operation("sms", 0.01f)
    )
  )

  "Calls count" should "be equal to count in selected range" in {
    db.callsInInterval(LocalDateTime.MIN, LocalDateTime.MAX).size should be(2)
    db.callsInInterval(LocalDateTime.parse("2019-11-20T11:59:58"), LocalDateTime.MAX).size should be(1)
  }

  "User action" should "be same" in {
    db.userActions(User("Alex", "Kane"), LocalDateTime.MIN, LocalDateTime.MAX).get should be(Iterable(db.actions(1), db.actions(2)))
    db.userActions(User("Alex", "Bane"), LocalDateTime.MIN, LocalDateTime.MAX).get should be(Iterable(db.actions(0)))
  }

  "Sum cost" should "be same to cost in selected range" in {
    db.sumCost(LocalDateTime.MIN, LocalDateTime.MAX) should equal((0.1 * 2 + 1 * 1 + 0.01 * 3) +- 0.000001)
    db.sumCost(LocalDateTime.parse("2019-11-20T10:55:59"), LocalDateTime.MAX) should equal((0.1 * 2 + 1.0 * 1) +- 0.000001)
    db.sumCost(LocalDateTime.parse("2019-11-20T10:59:59"), LocalDateTime.MAX) should equal((1.0 * 1) +- 0.000001)
    db.sumCost(LocalDateTime.parse("2019-11-20T11:59:59"), LocalDateTime.MAX) should equal(0.0 +- 0.000001)
  }

  "Avg calls duration" should "be same" in {
    db.avgCallsDuration should equal(1.5 +- 0.000001)
  }

}
