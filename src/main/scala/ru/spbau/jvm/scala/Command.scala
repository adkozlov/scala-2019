package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.commands.{Actions, ActionsCost, Avg, Calls, Help, Number, RegisteredUsers, Total}

trait Command {
  val name: String
  val help: String

  def execute(database: BillingDatabase, args: Array[String]): String
}

object CommandManager {
  val registeredCommands: List[Command] = List(Actions, Avg, Calls, Help, Number, Total, RegisteredUsers, ActionsCost)

  def runCommand(database: BillingDatabase, array: Array[String]): String =
    registeredCommands
      .find(_.name == array.head)
      .map(_.execute(database, array.drop(1)))
      .getOrElse {
        s"command ${array.head} not found"
      }
}