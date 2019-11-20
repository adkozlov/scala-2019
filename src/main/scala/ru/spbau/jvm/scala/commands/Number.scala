package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Number extends Command {
  override val name: String = "number"
  override val help: String = "номера телефонов заданного сотрудника"

  override def execute(database: BillingDatabase, args: Array[String]): String =
    database.userPhone(ParserUtil.parseUser(args))
      .map(_.map(_.value)).map(_.mkString("\n"))
      .getOrElse("USER NOT FOUND")
}
