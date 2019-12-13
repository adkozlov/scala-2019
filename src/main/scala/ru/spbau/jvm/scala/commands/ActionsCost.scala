package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object ActionsCost extends Command {
  override val name: String = "cost"
  override val help: String = "стоимость услуг связи конкретного абонента за период (опционально)"

  override def execute(database: BillingDatabase, args: Array[String]): String = {
    val user = ParserUtil.parseUser(args.take(2))
    val interval = ParserUtil.parseInterval(args.drop(2))
    database.userActionsCost(user, interval._1, interval._2).map(_.toString).getOrElse("USER NOT FOUND")
  }

}
