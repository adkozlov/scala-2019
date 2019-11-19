package ru.spbau.jvm.scala.cli.commands

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.jvm.scala.db.BillingDB

class TotalCommand extends Command {
  private val dateTimeFormat: DateTimeFormatter = DateTimeFormat.forPattern("dd.MM.YYYY")

  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    if (args.length != 5) {
      return ("Invalid arguments of calls function", List())
    }
    var from: DateTime = null
    var to: DateTime = null
    try {
      from = DateTime.parse(args(2), dateTimeFormat)
      to = DateTime.parse(args(4), dateTimeFormat)
    } catch {
      case _: Throwable => return ("Invalid date format, expected 'dd.MM.YYYY'", List())
    }
    val result = db.calls
      .filter(call => DateTime.parse(call.dateTime).isAfter(from) &&
        DateTime.parse(call.dateTime).isBefore(to))
      .foldLeft(0d)(_ + _.cost)
    ("total", List(s"$result"))

  }

  override def info(): String = "total from DATETIME to DATETIME -- суммарная стоимость услуг связи за заданный промежуток времени"

  override def name(): String = "total"

}
