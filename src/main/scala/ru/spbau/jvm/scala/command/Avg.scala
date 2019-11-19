package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.orm.Call

object Avg extends Command {
  override val name: String = "avg"

  override val help: String = "average time of calls"

  override def run(storage: Billing, args: Array[String]): String = {
    val res = storage.calls.map((it: Call) => it.duration).sum / storage.calls.size
    s"${res.toString}s"
  }
}
