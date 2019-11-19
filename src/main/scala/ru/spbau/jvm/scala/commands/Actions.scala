package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Actions extends Command {
  override val name: String = "actions"
  override val help: String = "список всех действий абонента за заданный промежуток времени"

  override def execute(database: BillingDatabase, args: Array[String]): String = {
    val user = ParserUtil.parseUser(args.take(2))
    val interval = ParserUtil.parseInterval(args.drop(2))
    // TODO pretty print
    database.userActions(user, interval._1, interval._2).toString
  }
}
