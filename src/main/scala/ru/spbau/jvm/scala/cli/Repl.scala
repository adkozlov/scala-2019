package ru.spbau.jvm.scala.cli

import ru.spbau.jvm.scala.billing._

import scala.io.BufferedSource
import scala.util.{Failure, Success}

object Repl {
  def process(context: BillingSystem, source: BufferedSource): Unit = {
    print("# ")
    for (line <- source.getLines()) {
      val query = QueryParser.parse(line)
      query match {
        case Success(command) =>
          try {
            command.execute(context)
          } catch {
            case e: BillingSystemPersonNotFoundException => println(e.getMessage)
            case e: BillingNoContactNotFoundException => println(e.getMessage)
            case e: BillingSystemEmptyDateRangeException => println(e.getMessage)
            case _ => println("Unable to execute the command!")
          }
        case Failure(ex) =>
          if (ex.getMessage != null)
            println(s"Error: ${ex.getMessage}")
          else
            println("Error: unable to execute the command.")
      }
      print("# ")
    }
  }
}
