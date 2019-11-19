package ru.spbau.jvm.scala.cli.commands

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.db.BillingDB
import ru.spbau.jvm.scala.db.rows.{CallsRow, PhoneRow, UserRow}

class CallsCommandSpec extends FlatSpec with Matchers {
  val db = new BillingDB
  val callsCommand = new CallsCommand

  "Calls command name" should "be \"calls\"" in {
    callsCommand.name should be("calls")
  }

  "Calls command info" should
    "be \"calls from datetime to datetime -- список всех звонков за заданный промежуток времени\"" in {
    callsCommand.info should be("calls from datetime to datetime -- список всех звонков за заданный промежуток времени")
  }

  "Calls command" should "return list of calls" in {
    db.calls += CallsRow("1", "123", "321", "2019-09-16T12:53:51+0000", 10, 0)
    db.calls += CallsRow("2", "321", "123", "2019-10-12T14:54:53+0000", 2, 4)
    db.calls += CallsRow("3", "123", "321", "2019-10-14T13:52:54+0000", 3, 2)
    db.phones += PhoneRow("1", "1", "123")
    db.phones += PhoneRow("2", "2", "321")
    db.users += UserRow("1", "FirstName1", "LastName1")
    db.users += UserRow("2", "FirstName2", "LastName2")
    callsCommand.execute(db, Array("calls", "from", "11.10.2019", "to", "15.10.2019")) should be(
      ("FirstName | LastName | Callee | Duration (s) | Cost ($)",
        Iterable[String](
          "FirstName2 | LastName2 | 321 | 2.0 | 4.0",
          "FirstName1 | LastName1 | 123 | 3.0 | 2.0"
        ))
    )
  }
}
