package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.BillingDB

trait Command {
  def execute(db: BillingDB, args: Array[String]): (String, Iterable[String])

  def name(): String

  def info(): String
}
