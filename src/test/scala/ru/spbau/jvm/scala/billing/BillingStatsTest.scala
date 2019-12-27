package ru.spbau.jvm.scala.billing

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest._

class BillingStatsTest extends FlatSpec {

  import BillingStatsTest._

  behavior of "BillingStats"

  it should "return calls from correctly" in {
    assert(billingStats.getCalls(date("04.11.2019"), None) == List(
      Call(
        Employee("Jazmyn", "Hansen"),
        PhoneNumber("320(45)352-71-41"),
        dateTime("04.11.2019 03:43:58"),
        75,
        0.75
      )
    ))
  }

  it should "return calls from to correctly" in {
    assert(billingStats.getCalls(date("27.04.2019"), date("27.05.2019")) == List(
      Call(
        Employee("Alton", "Khan"),
        PhoneNumber("03(265)568-07-84"),
        dateTime("27.04.2019 01:00:53"),
        21,
        0.21
      ),
      Call(
        Employee("Ella-May", "Cherry"),
        PhoneNumber("9(9203)020-79-33"),
        dateTime("18.05.2019 14:36:14"),
        666,
        6.66)
    ))
  }

  it should "return outgoing calls correctly" in {
    assert(billingStats.getOutgoingCalls(Employee("Humera", "Acosta")) == List(
      Call(
        Employee("Humera", "Acosta"),
        PhoneNumber("43(4885)700-32-94"),
        dateTime("10.06.2019 02:07:17"),
        92,
        0.92
      ),
      Call(
        Employee("Humera", "Acosta"),
        PhoneNumber("75(5791)483-41-01"),
        dateTime("03.07.2019 07:11:28"),
        248,
        2.48
      )
    ))
  }

  it should "return incoming calls correctly" in {
    assert(billingStats.getIncomingCalls(Employee("Jazmyn", "Hansen")) == List(
      Call(
        Employee("Ari", "Alfaro"),
        PhoneNumber("0(9158)369-15-27"),
        dateTime("07.09.2019 18:59:50"),
        21,
        0.21
      )
    ))
  }

  it should "get number correctly" in {
    assert(billingStats.getNumber(Employee("Ari", "Alfaro")) == PhoneNumber("52(4373)767-23-07"))
  }

  it should "get owner correctly" in {
    assert(billingStats.getOwner(PhoneNumber("0(9158)369-15-27")).contains(Employee("Jazmyn", "Hansen")))
  }

  it should "return None if number is reserved but has no owner" in {
    assert(billingStats.getOwner(PhoneNumber("393(002)750-21-29")).isEmpty)
  }

  it should "count average duration correctly" in {
    assert(billingStats.getAverageDuration == 184)
  }

  it should "count total cost correctly" in {
    assert(billingStats.getTotalCost(None, None) == 23.94)
  }

  it should "count total cost during period correctly" in {
    assert(billingStats.getTotalCost(date("27.04.2019"), date("27.05.2019")) == 6.87)
  }

  it should "throw NoSuchElementException if looking for number of non-existing employee" in {
    assertThrows[NoSuchElementException](billingStats.getNumber(Employee("Maria", "Malysheva")))
  }

  it should "throw NoSuchElementException if looking for outgoing calls of non-existing employee" in {
    assertThrows[NoSuchElementException](billingStats.getOutgoingCalls(Employee("Maria", "Malysheva")))
  }

  it should "throw NoSuchElementException if looking for incoming calls of non-existing employee" in {
    assertThrows[NoSuchElementException](billingStats.getIncomingCalls(Employee("Maria", "Malysheva")))
  }

  it should "throw NoSuchElementException if looking for owner of non-reserved number" in {
    assertThrows[NoSuchElementException](billingStats.getOwner(PhoneNumber("8-800-555-35-35")))
  }
}

object BillingStatsTest {
  private val billingStats = BillingStats(new File(getClass.getResource("/").getPath))
  private val dateFormat = new SimpleDateFormat("dd.MM.yyyy")
  private val dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

  private def date(date: String): Option[Date] = {
    Some(dateFormat.parse(date))
  }

  private def dateTime(date: String): Date = {
    dateTimeFormat.parse(date)
  }
}