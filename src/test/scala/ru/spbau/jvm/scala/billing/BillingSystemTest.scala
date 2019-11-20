package ru.spbau.jvm.scala.billing

import org.scalatest.FlatSpec
import ru.spbau.jvm.scala.cli.{DateRange, PersonName}
import ru.spbau.jvm.scala.utils.Utils

class BillingSystemTest extends FlatSpec {
  private val testDbPath = "./src/test/resources/databaseTest"
  private val testFiles = Utils.getTxtFilesFromDirectory(testDbPath)
  private val billingSystem = new BillingSystem(testFiles)

  private val noneRange = DateRange(None, None)

  /*
   * avg [duration | cost] [from <date> [<date-to>]] -- add average call cost
   */
  "avg duration" should "do smth" in {
    billingSystem.avgDuration(noneRange)
  }

  "avg cost" should "do smth" in {
    billingSystem.avgCost(noneRange)
  }

  /*
   * calls [from <date-from> [<date-to>]] -- all calls
   */
  "calls" should "do smth" in {
    billingSystem.calls(noneRange)
  }

  /*
   * total [from <date-from> [<date-to>]] -- total cost
   */
  "total" should "do smth" in {
    billingSystem.total(noneRange)
  }

  /*
   * number <first-name> <last-name> -- number of an employee <first-name> <last-name>
   */
  "number" should "do smth" in {
    billingSystem.number(PersonName("John", "Doe"))
  }

  /*
   * messages [from <date-from> [<date-to>]] -- all messages
   */
  "messages" should "do smth" in {

  }

  /*
   * contacts <name> to <name> [from <date-from [to <date-to>]] -- count of contacts between two people
   */
  "contacts" should "do smth" in {

  }
}
