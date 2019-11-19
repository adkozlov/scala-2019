package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.BillingDB

class AvgCommand extends Command {
  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    if (args.length != 1) {
      return ("Invalid arguments of calls function", List())
    }
    val result: Double = db.calls.foldLeft(0d)(_ + _.duration) / db.calls.size
    ("avg", List(s"${result}s"))
  }

  override def info(): String =
    "avg -- средняя длительность звонка"

  override def name(): String = "avg"
}
