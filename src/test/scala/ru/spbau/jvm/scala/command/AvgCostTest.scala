package ru.spbau.jvm.scala.command

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.storage.db.orm.Call

class AvgCostTest extends FlatSpec with Matchers {
  "AvgCost" should "calc avg cost" in {
    val calls = List(
      new Call(new Row("1,,,2019-11-19T00:00:02,10,1")),
      new Call(new Row("2,,,2019-11-19T00:00:00,2,2")),
      new Call(new Row("3,,,2019-11-19T00:00:01,3,3"))
    )
    val db = new Billing(calls, List(), List())
    AvgCost.run(db, Array("avg-cost")) should be("2.0")
  }
}
