package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Calls extends Command {
  override val name: String = "calls"
  override val help: String = "список всех звонков за заданный промежуток времени"

  override def execute(database: BillingDatabase, args: Array[String]): String = {
    val interval = ParserUtil.parseInterval(args)
    //TODO pretty print
    database.callsInInterval(interval._1, interval._2).toString
  }

}
