package ru.spbau.jvm.scala.cli

import ru.spbau.jvm.scala.cli.commands.{CallsFromDateTimeRange, Command}
import ru.spbau.jvm.scala.db.DB

class Invoker(db: DB) {
  private var nameToCommand: Map[String, _ <: Command] =
    Map("CallsFromDateTimeRange" -> new CallsFromDateTimeRange)

  def register(commandName: String, command: Command): Unit = {
    nameToCommand += (commandName -> command)
  }

  def exec(commandName: String, args: Array[String]): String = {
    nameToCommand.get(commandName) match {
      case Some (command) =>
        command.exec(db, args)
      case None =>
        "Error command: " + commandName
    }
  }
}
