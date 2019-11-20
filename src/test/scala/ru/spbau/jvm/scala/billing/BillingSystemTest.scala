package ru.spbau.jvm.scala.billing

import java.text.SimpleDateFormat

import org.scalatest.FlatSpec
import ru.spbau.jvm.scala.cli.{DateRange, PersonName}
import ru.spbau.jvm.scala.utils.Utils

class BillingSystemTest extends FlatSpec {
  private val testDbPath = "./src/test/resources/databaseTest"
  private val testFiles = Utils.getTxtFilesFromDirectory(testDbPath)
  private val billingSystem = new BillingSystem(testFiles)

  private val noneRange = DateRange()
  private val johnDoe = PersonName("John", "Doe")
  private val martingKing = PersonName("Martin", "King")
  private val dateParser = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  /*
   * avg [duration | cost] [from <date> [<date-to>]] -- add average call cost
   */
  "avg duration" should "do smth" in {
    assert(billingSystem.avgDuration(noneRange) == 10)
  }

  "avg cost" should "do smth" in {
    assert(billingSystem.avgCost(noneRange) == 20.0)
  }

  /*
   * calls [from <date-from> [<date-to>]] -- all calls
   */
  "calls" should "do smth" in {
    val expected =
      List(List("John", "Doe", "+79000000001", 10, 20.0),
        List("John", "Doe", "+79000000002", 10, 20.0),
        List("John", "Doe", "+79000000001", 10, 20.0),
        List("John", "Doe", "+79000000002", 10, 20.0))

    assert(billingSystem.calls(noneRange) == expected)
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
  "total" should "do smth" in {
    assert(billingSystem.total(noneRange) == 83.0)
  }

  /*
   * number <first-name> <last-name> -- number of an employee <first-name> <last-name>
   */
  "number" should "do smth" in {
    assert(billingSystem.number(johnDoe) == List("+79000000000", "+79000000001"))
  }

  /*
   * messages [from <date-from> [<date-to>]] -- all messages
   */
  "messages" should "do smth" in {
    assert(billingSystem.messages(johnDoe, noneRange) == SmsStat(0, 1))
    assert(billingSystem.messages(martingKing, noneRange) == SmsStat(1, 0))
  }

  /*
   * contacts <name> to <name> [from <date-from [to <date-to>]] -- count of contacts between two people
   */
  "contacts" should "do smth" in {
    assert(billingSystem.contact(johnDoe, martingKing, noneRange) == 2)
    assert(billingSystem.contact(martingKing, johnDoe, noneRange) == 1)
  }
}
