package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.BillingDB

class HelpCommand(val commands: Iterable[Command]) extends Command {
  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    ("help", commands.map(_.info()))
  }

  override def name(): String = "help"

  override def info(): String = "help -- вызов справки"
}
