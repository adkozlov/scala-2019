package ru.spbau.jvm.scala.cli.commands

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.db.BillingDB
import ru.spbau.jvm.scala.db.rows.CallsRow

class TotalCommandSpec extends FlatSpec with Matchers {
  val db = new BillingDB
  val totalCommand = new TotalCommand

  "Total command name" should "be \"total\"" in {
    totalCommand.name should be("total")
  }

  "Total command info" should "be \"total -- средняя длительность звонка\"" in {
    totalCommand.info should be(
      "total from DATETIME to DATETIME -- суммарная стоимость услуг связи за заданный промежуток времени"
    )
  }

  "Total command" should "calc total cost of calls" in {
    db.calls += CallsRow("1", "123", "321", "2019-09-16T12:53:51+0000", 10, 0)
    db.calls += CallsRow("2", "321", "123", "2019-10-12T14:54:53+0000", 2, 4)
    db.calls += CallsRow("3", "123", "321", "2019-10-14T13:52:54+0000", 3, 3)
    totalCommand.execute(db, Array("total", "from", "11.10.2019", "to", "15.10.2019")) should
      be(("total", Iterable[String]("7.0")))
  }
}
