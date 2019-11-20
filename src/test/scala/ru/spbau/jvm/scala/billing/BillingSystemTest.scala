package ru.spbau.jvm.scala.billing

import java.text.SimpleDateFormat

import org.scalatest.FlatSpec
import ru.spbau.jvm.scala.cli.{DateRange, PersonName}
import ru.spbau.jvm.scala.utils.Utils

class BillingSystemTest extends FlatSpec {
  private val testDbPath = "./src/test/resources/databaseTest"
  private val testFiles = Utils.getTxtFilesFromDirectory(testDbPath)
  private val billingSystem = new BillingSystem(testFiles)

  private val dateParser = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  private val noneRange = DateRange()
  private val badRange = DateRange(Some(dateParser.parse("2019-11-01 18:00")),
    Some(dateParser.parse("2019-11-01 11:00")))

  private val johnDoe = PersonName("John", "Doe")
  private val martingKing = PersonName("Martin", "King")
  private val noOne = PersonName("No", "One")

  /*
   * avg [duration | cost] [from <date> [<date-to>]] -- add average call cost
   */
  "avg duration" should "pass" in {
    assert(billingSystem.avgDuration(noneRange) == 10)
  }

  "avg duration with strange range" should "fail" in {
    assertThrows[BillingSystemEmptyDateRangeException](billingSystem.avgDuration(badRange))
  }

  "avg cost" should "pass" in {
    assert(billingSystem.avgCost(noneRange) == 20.0)
  }

  /*
   * calls [from <date-from> [<date-to>]] -- all calls
   */
  "calls" should "pass" in {
    val expected =
      List(List("John", "Doe", "+79000000001", 10, 20.0),
        List("John", "Doe", "+79000000002", 10, 20.0),
        List("John", "Doe", "+79000000001", 10, 20.0),
        List("John", "Doe", "+79000000002", 10, 20.0))

    assert(billingSystem.calls(noneRange) == expected)
  }

  "calls with strange range" should "fail" in {
    assertThrows[BillingSystemEmptyDateRangeException](billingSystem.calls(badRange))
  }

  "`calls` from date" should "pass" in {
    val dateFrom = dateParser.parse("2019-11-01 14:00")
    val expected =
      List(List("John", "Doe", "+79000000002", 10, 20.0),
        List("John", "Doe", "+79000000001", 10, 20.0))

    assert(billingSystem.calls(DateRange(Some(dateFrom))) == expected)
  }

  "`calls` util date" should "pass" in {
    val dateTo = dateParser.parse("2019-11-01 14:00")
    val expected =
      List(
        List("John", "Doe", "+79000000001", 10, 20.0),
        List("John", "Doe", "+79000000002", 10, 20.0))

    assert(billingSystem.calls(DateRange(None, Some(dateTo))) == expected)
  }

  "`calls` from date util another date" should "pass" in {
    val dateFrom = dateParser.parse("2019-11-01 11:00")
    val dateTo = dateParser.parse("2019-11-01 14:00")
    val expected =
      List(
        List("John", "Doe", "+79000000001", 10, 20.0),
        List("John", "Doe", "+79000000002", 10, 20.0))

    assert(billingSystem.calls(DateRange(Some(dateFrom), Some(dateTo))) == expected)
  }

  /*
   * total [from <date-from> [<date-to>]] -- total cost
   */
  "total" should "pass" in {
    assert(billingSystem.total(noneRange) == 83.0)
  }

  "total with strange range" should "fail" in {
    assertThrows[BillingSystemEmptyDateRangeException](billingSystem.total(badRange))
  }

  /*
   * number <first-name> <last-name> -- number of an employee <first-name> <last-name>
   */
  "number" should "pass" in {
    assert(billingSystem.number(johnDoe) == List("+79000000000", "+79000000001"))
  }

  "number of No One" should "fail" in {
    assertThrows[BillingSystemPersonNotFoundException](billingSystem.number(noOne))
  }

  /*
   * messages <person> [from <date-from> [<date-to>]] -- all messages
   */
  "messages" should "pass" in {
    assert(billingSystem.messages(johnDoe, noneRange) == SmsStat(0, 1))
    assert(billingSystem.messages(martingKing, noneRange) == SmsStat(1, 0))
  }

  "messages of No One" should "fail" in {
    assertThrows[BillingSystemPersonNotFoundException](billingSystem.messages(noOne, noneRange))
  }

  "messages with strange range" should "fail" in {
    assertThrows[BillingSystemEmptyDateRangeException](billingSystem.messages(johnDoe, badRange))
  }

  /*
   * contacts <name> to <name> [from <date-from [to <date-to>]] -- count of contacts between two people
   */
  "contacts" should "pass" in {
    assert(billingSystem.contact(johnDoe, martingKing, noneRange) == 2)
    assert(billingSystem.contact(martingKing, johnDoe, noneRange) == 1)
  }

  "contacts of No One with someone" should "fail" in {
    assertThrows[BillingSystemPersonNotFoundException](billingSystem.contact(noOne, johnDoe, noneRange))
  }

  "contacts of someone with NoOne" should "fail" in {
    assertThrows[BillingSystemPersonNotFoundException](billingSystem.contact(johnDoe, noOne, noneRange))
  }

  "contacts with strange range" should "fail" in {
    assertThrows[BillingSystemEmptyDateRangeException](billingSystem.contact(johnDoe, martingKing, badRange))
  }
}
