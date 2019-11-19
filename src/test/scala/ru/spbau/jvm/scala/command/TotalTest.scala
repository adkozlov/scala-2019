package ru.spbau.jvm.scala.command

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.storage.db.orm.Call

class TotalTest extends FlatSpec with Matchers {
  "Total" should "calc return total cost of all calls" in {
    val calls = List(
      new Call(new Row("1,,,2019-11-16T00:00:02,10,1")),
      new Call(new Row("2,,,2019-11-17T00:00:00,2,2")),
      new Call(new Row("3,,,2019-11-18T00:00:01,3,3")),
      new Call(new Row("3,,,2019-11-19T00:00:01,3,4"))
    )
    val db = new Billing(calls, List(), List())
    Total.run(db, Array("total", "from", "16.11.2019", "to", "18.11.2019")) should be("6.0")
  }
}
