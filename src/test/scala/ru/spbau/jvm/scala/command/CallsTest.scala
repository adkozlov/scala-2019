package ru.spbau.jvm.scala.command

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.storage.db.orm.{Call, Phone, User}

class CallsTest extends FlatSpec with Matchers {
  "Calls" should "return list of calls" in {
    val phones = List(
      new Phone(new Row("1, 1, 123")),
      new Phone(new Row("2, 2, 456"))
    )

    val users = List(
      new User(new Row("1, Will, Smith")),
      new User(new Row("2, Steve, Spielberg"))
    )
    val calls = List(
      new Call(new Row("1,123,123,2019-11-19T00:00:02,10,0")),
      new Call(new Row("2,456,123,2019-11-20T00:00:04,2,0")),
      new Call(new Row("3,123,456,2019-11-21T00:00:01,3,0"))
    )

    val db = new Billing(calls, phones, users)

    Calls.run(db, Array("calls", "from", "20.11.2019", "to", "21.11.2019")) should be(
      "Steve | Spielberg | 456 | 2 | 0.0\nWill | Smith | 123 | 3 | 0.0"
    )
  }
}
