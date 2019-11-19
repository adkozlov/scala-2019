package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Number extends Command {
  override val name: String = "number"
  override val help: String = "номера телефонов заданного сотрудника"

  override def execute(database: BillingDatabase, args: Array[String]): String =
    //TODO pretty print
    database.userPhone(ParserUtil.parseUser(args)).toString

}
