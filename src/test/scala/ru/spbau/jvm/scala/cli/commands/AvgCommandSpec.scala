package ru.spbau.jvm.scala.cli.commands

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.db.BillingDB
import ru.spbau.jvm.scala.db.rows.CallsRow

class AvgCommandSpec extends FlatSpec with Matchers {
  val db = new BillingDB
  val avgCommand = new AvgCommand

  "Avg command name" should "be \"avg\"" in {
    avgCommand.name should be("avg")
  }

  "Avg command info" should "be \"avg -- средняя длительность звонка\"" in {
    avgCommand.info should be("avg -- средняя длительность звонка")
  }

  "Avg command" should "calc avg call from db" in {
    db.calls += new CallsRow("1", "", "", "", 10, 0)
    db.calls += new CallsRow("2", "", "", "", 2, 0)
    db.calls += new CallsRow("3", "", "", "", 3, 0)
    avgCommand.execute(db, Array("avg")) should be(("avg", List("5.0s")))
  }
}
