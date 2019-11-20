package ru.spbau.jvm.scala.cli

import ru.spbau.jvm.scala.billing.BillingSystem
import ru.spbau.jvm.scala.utils.Utils

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val databaseFiles = Utils.getTxtFilesFromDirectory("resources/database2")
      val billingSystem = new BillingSystem(databaseFiles)
      Repl.process(billingSystem, Source.stdin)
    } catch {
      case e: Exception =>
        if (e.getMessage.isEmpty) println("Fatal error")
        else println("Fatal Error: " + e.getMessage)
    }
  }
}
