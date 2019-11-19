package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.cli.CLI
import ru.spbau.jvm.scala.db.BillingDB

object Main {

  def main(args: Array[String]): Unit = {
    val db = new BillingDB
    db.loadTables("resources")
    val cli = new CLI(db)
    cli.start()
  }
}
