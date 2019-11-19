package ru.spbau.jvm.scala.command
import ru.spbau.jvm.scala.storage.Billing

object Help extends Command {
  override val name: String = "help"

  override val help: String = "help message"

  override def run(storage: Billing, args: Array[String]): String = Command.help
}
