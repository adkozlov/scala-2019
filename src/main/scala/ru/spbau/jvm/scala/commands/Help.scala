package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command, CommandManager}

object Help extends Command {
  override val name: String = "help"
  override val help: String = "вызов справки"

  override def execute(database: BillingDatabase, args: Array[String]): String =
    CommandManager.registeredCommands.map(command => s"> ${command.name} - ${command.help}").mkString("\n")
}
