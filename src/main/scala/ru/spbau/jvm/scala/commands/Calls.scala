package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Calls extends Command {
  override val name: String = "calls"
  override val help: String = "список всех звонков за заданный промежуток времени"

  override def execute(database: BillingDatabase, args: Array[String]): String = {
    val interval = ParserUtil.parseInterval(args)
    database.callsInInterval(interval._1, interval._2)
      .map { case (user, phone, sec, cost) =>
        s"${user.map(_.firstName).getOrElse("UNKNOWN")} | " +
          s"${user.map(_.lastName).getOrElse("UNKNOWN")} | " +
          s"${phone.map(_.value).getOrElse("UNKNOWN")} | " +
          s"$sec | " +
          s"${cost.map(_.toString).getOrElse("UNKNOWN")}"
      }
      .mkString("\n")
  }

}
