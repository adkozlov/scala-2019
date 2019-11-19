package ru.spbau.jvm.scala.cli.commands

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.jvm.scala.db.BillingDB

class CallsCommand extends Command {
  private val dateTimeFormat: DateTimeFormatter = DateTimeFormat.forPattern("dd.MM.YYYY")

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
    val calls = db.calls
      .filter(call => DateTime.parse(call.dateTime).isAfter(from) &&
        DateTime.parse(call.dateTime).isBefore(to))
    val callsWithPhones = calls
      .map(call => (call, db.phones.find(phone => phone.phoneNumber == call.phoneNumberFrom).get))
    val phones = callsWithPhones
      .map(
        callUser => (callUser._1, callUser._2, db.users.find(user => user.id == callUser._2.userId)
          .get)
      )
    val value: Iterable[String] = phones.map(callUserPhone =>
      Array(callUserPhone._3.firstName, callUserPhone._3.lastName,
        callUserPhone._1.phoneNumberFrom, callUserPhone._1.duration, callUserPhone._1.cost)
        .mkString(" | ")
    )
    ("FirstName | LastName | Callee | Duration (s) | Cost ($)", value)
  }

  override def info(): String =
    "calls from datetime to datetime -- список всех звонков за заданный промежуток времени"

  override def name(): String = "calls"

}
