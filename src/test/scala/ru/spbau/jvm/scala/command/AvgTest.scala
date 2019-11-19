package ru.spbau.jvm.scala.command

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.storage.db.orm.Call

class AvgTest extends FlatSpec with Matchers {
  "Avg" should "calc avg duration" in {
    val calls = List(
      new Call(new Row("1,,,2019-11-19T00:00:02,10,0")),
      new Call(new Row("2,,,2019-11-19T00:00:00,2,0")),
      new Call(new Row("3,,,2019-11-19T00:00:01,3,0"))
    )
    val db = new Billing(calls, List(), List())
    Avg.run(db, Array("avg")) should be("5s")
  }
}
