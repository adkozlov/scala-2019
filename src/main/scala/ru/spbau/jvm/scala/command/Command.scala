package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing

trait Command {
  val name: String

  val help: String

  def run(storage: Billing, args: Array[String]): String
}

object Command {
  private val commands = List(Avg, AvgCost, Calls, Employee, Number, Total, TotalDuration, Help)

  val help: String = commands.map((it: Command) => s"${it.name} - ${it.help}").mkString("\n")

  def get(name: String): Command = commands.find((cm: Command) => cm.name == name).getOrElse(throw new IllegalArgumentException(s"command ${name} not found"))
}
