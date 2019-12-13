package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object RegisteredUsers extends Command {
  override val name: String = "users"
  override val help: String = "список пользователей"

  override def execute(database: BillingDatabase, args: Array[String]): String =
    args match {
      case Array() => database.registeredUsers.map { user => s"${user.firstName} ${user.lastName}" }.mkString("\n")
      case _ => throw new IllegalArgumentException("Parse error")
    }
}
