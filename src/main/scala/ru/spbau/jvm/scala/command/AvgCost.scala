package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.orm.Call

object AvgCost extends Command {
  override val name: String = "avg-cost"

  override val help: String = "average cost of calls"

  override def run(storage: Billing, args: Array[String]): String = {
    val res = storage.calls.map((it: Call) => it.cost).sum / storage.calls.size
    s"${res.toString}"
  }
}
