package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.inmemory.InMemoryBillingDatabase

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val db: BillingDatabase = new InMemoryBillingDatabase("resources")
    var alive = true
    while (alive) {
      StdIn.readLine match {
        case null => alive = false
        case line => line.split("\\s").filter(_.nonEmpty).map(_.trim) match {
          case Array() => ()
          case other => try {
            println(CommandManager.runCommand(db, other))
          } catch {
            case e: IllegalArgumentException => println(e.getMessage)
          }
        }
      }
    }
  }
}
