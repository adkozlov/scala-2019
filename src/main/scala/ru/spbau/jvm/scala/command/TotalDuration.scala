package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.orm.Call
import ru.spbau.jvm.scala.util.Interval

object TotalDuration extends Command {
  override val name: String = "total-duration"

  override val help: String = "total duration of calls"

  override def run(storage: Billing, args: Array[String]): String = {
    val (from, to) = Interval.parse(args.drop(1))

    val res = Interval.filterCalls(from, to, storage.calls)
      .map((it: Call) => it.duration)
      .sum

    s"${res.toString}"
  }
}
