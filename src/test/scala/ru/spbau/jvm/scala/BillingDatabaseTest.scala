package ru.spbau.jvm.scala

import java.text.SimpleDateFormat

import org.scalatest._

class BillingDatabaseTest extends FlatSpec with Matchers {
  BillingDatabase.loadDatabase()
  val format = new SimpleDateFormat("dd.MM.yyyy")

  "Get calls between dates" should "return correct list of calls" in {
    val expected = List[Call](
      Call(Employee("Belinda", "Francis"), "+7 900 123 45 67", format.parse("16.11.2019"), 547, 5.47),
      Call(Employee("Belinda", "Francis"), "+7 900 123 45 67", format.parse("17.11.2019"), 390, 3.9))
    BillingDatabase.getCallsBetweenDates(format.parse("16.11.2019"), format.parse("18.11.2019")) should be (expected)
  }

  "Get average call duration" should "be correct" in {
    BillingDatabase.getAverageCallDuration should be (226)
  }

  "Get total call cost" should "be correct" in {
    BillingDatabase.getTotalCallCostBetweenDates(format.parse("16.11.2019"), format.parse("19.11.2019")) should be (10.63 +- 0.0001)
  }

  "Get employee phone" should "be correct" in {
    BillingDatabase.getEmployeePhone("Nella", "Mcknight") should be ("+7 900 642 23 29")
  }

  it should "throw IllegalArgumentException if employee has not got a phone number" in {
    a [IllegalArgumentException] should be thrownBy {
      BillingDatabase.getEmployeePhone("Gabriel", "Khan")
    }
  }

  "Get calls by employee" should "be correct" in {
    val expected = List[Call](
      Call(Employee("Yuvaan", "Moyer"), "+7 900 234 55 42", format.parse("19.11.2019"), 92 ,0.92))
    BillingDatabase.getCallsByEmployee("Yuvaan", "Moyer") should be (expected)
  }

  "Get employee with highest total cost" should "be correct" in {
    BillingDatabase.getEmployeeWithHighestTotalCost should be (Option(Employee("Belinda", "Francis"), 9.37))
  }
}
