package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing

trait Command {
  val help: String

  def run(storage: Billing, args: Array[String]): String
}

object Command {
  private val commands = List(Avg, Calls, Number, Total, Help)

  val help: String = commands.map(_.help).mkString("\n")

  def get(name: String): Command = {
    name match {
      case "avg" => Avg
      case "calls" => Calls
      case "number" => Number
      case "total" => Total
      case "help" => Help
      case _ => throw new IllegalArgumentException(s"command ${name} not found")
    }
  }
}
