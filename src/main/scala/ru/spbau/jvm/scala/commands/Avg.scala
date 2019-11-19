package ru.spbau.jvm.scala.commands

import ru.spbau.jvm.scala.{BillingDatabase, Command}

object Avg extends Command {
  override val name: String = "avg"
  override val help: String = "средняя длительность звонка"

  override def execute(database: BillingDatabase, args: Array[String]): String =
    args match {
      case Array() => database.avgCallsDuration.toString
      case _ => throw new IllegalArgumentException("Parse error")
    }
}
