package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.BillingDB

class UnknownCommand extends Command {
  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    (s"Unknown command ${args(0)}", List())
  }

  override def name(): String = ???

  override def info(): String = ???
}
