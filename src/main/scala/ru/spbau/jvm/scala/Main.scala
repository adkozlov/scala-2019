package ru.spbau.jvm.scala

import java.time.LocalDateTime

import scala.io.StdIn;

object Main {

  def main(args: Array[String]): Unit = {
    var toContinue = true
    val parser = new CommandParser()
    //val dateTime: LocalDateTime = LocalDateTime.parse("2016-04-17")
    while (toContinue) {
      val command = StdIn.readLine()
      toContinue = parser.parseCommand(command)
    }
  }
}
