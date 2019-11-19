package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Total extends Command {
  override val name: String = "total"
  override val help: String = "суммарная стоимость услуг связи за заданный промежуток времени"

  override def execute(database: BillingDatabase, args: Array[String]): String = {
    val interval = ParserUtil.parseInterval(args)
    database.sumCost(interval._1, interval._2).toString
  }
}
