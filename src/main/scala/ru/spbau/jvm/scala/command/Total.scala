package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.orm.Call
import ru.spbau.jvm.scala.util.Interval

object Total extends Command {
  override val name: String = "total"

  override val help: String = "total cost of calls"

  override def run(storage: Billing, args: Array[String]): String = {
    val (from, to) = Interval.parse(args.drop(1))

    val res = Interval.filterCalls(from, to, storage.calls)
      .map((it: Call) => it.cost)
      .sum

    s"${res.toString}"
  }
}
