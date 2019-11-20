package ru.spbau.jvm.scala.cli.commands

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.jvm.scala.db.BillingDB

class LongestCallCommand extends Command{

  private val dateTimeFormat: DateTimeFormatter = DateTimeFormat.forPattern("dd.MM.YYYY")
  private val header = "Phone number from | Phone number to | Duration (s) | Cost ($)"

  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    if (args.length < 5) {
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
    val longestCall = db.calls
      .filter(call => DateTime.parse(call.dateTime).isAfter(from) &&
        DateTime.parse(call.dateTime).isBefore(to))
      .maxBy(_.duration)
    if (longestCall == null) {
      return (header, List())
    }
    (header,
      List(List(longestCall.phoneNumberFrom, longestCall.phoneNumberTo, longestCall.duration.toString,
        longestCall.cost.toString).mkString(" | ")))
  }

  override def name(): String = "longest"

  override def info(): String = "longest from datetime to datetime -- самый длинный звонок за промежуток времени"
}
