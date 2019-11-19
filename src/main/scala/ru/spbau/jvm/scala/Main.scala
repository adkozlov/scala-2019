package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.cli.CLI
import ru.spbau.jvm.scala.db.DB
object Main {

  def main(args: Array[String]): Unit = {
    val db = new DB()
    db.loadTables("resources")
    new CLI(db).exec
  }
}
