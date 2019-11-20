package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.DB

trait Command {
  def exec(db: DB, args: Array[String]): String

  def getInfo(): String
}
