package ru.spbau.jvm.scala

import java.text.SimpleDateFormat

import org.scalatest._
import ru.spbau.jvm.scala.company_entity.User
import ru.spbau.jvm.scala.operator_entity.Call

class BillingDatabaseTest extends FlatSpec {
  BillingDatabase.loadDatabase()

  "Check state" should "not throw any exception" in {
    BillingDatabase.checkState()
  }

  "Average call length" should "be correct" in {
    assert(BillingDatabase.getAverageDuration === 246)
  }

  "Get phone by user" should "return empty if user has no phone" in {
    assert(BillingDatabase.getPhoneByUser(User("Darrell", "Swift")).isEmpty)
  }

  "Get phone by user" should "return correct phone if user has a phone" in {
    assert(BillingDatabase.getPhoneByUser(User("Amy", "Moreno")).get ===
    "+7 900 123 45 67")
  }

  "Get user by phone" should "return correct user if it is a correct phone" in {
    assert(BillingDatabase.getUserByPhone("+7 900 123 45 67") ===
      User("Amy", "Moreno"))
  }

  "Get calls between dates" should "return correct list of calls" in {
    val format = new SimpleDateFormat("dd-MM-yyyy")
    val expectedCalls = List[(User, Call)](
      (User("Amy", "Moreno"), Call("+7 900 123 45 67", format.parse("20-04-2019"), 200, 1.23)),
      (User("Amy", "Steadman"), Call("+7 900 123 45 68", format.parse("30-04-2019"), 100, 15)))
    assert(BillingDatabase.getCallsBetweenDates(
      format.parse("20-04-2019"), format.parse("01-05-2019")) ===
      expectedCalls)
  }

  "Get total cost between dates" should "be correct" in {
    val format = new SimpleDateFormat("dd-MM-yyyy")
    assert(BillingDatabase.getTotalCostBetweenDates(
      format.parse("20-04-2019"), format.parse("01-05-2019")) === 16.23)
  }

  "Get user with max total duration" should "be correct" in {
    assert(BillingDatabase.getUserWithMaxTotalDuration.get ===
      (User("Amy", "Steadman"), 900))
  }

  "Get user with max spending" should "be correct" in {
    assert(BillingDatabase.getUserMaxSpending.get ===
      (User("Amy", "Moreno"), 16.43))
  }
}
