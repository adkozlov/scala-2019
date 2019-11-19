package ru.spbau.jvm.scala

import java.io.File

import ru.spbau.jvm.scala.command.Command
import ru.spbau.jvm.scala.storage.Billing

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val billing = Billing.load(new File("resources/calls.txt"), new File("resources/phones.txt"), new File("resources/users.txt"))
    while (true) {
      val input = StdIn.readLine().split("\\s").filter(!_.isBlank).map(_.trim)
      val res = try {
        val command = Command.get(input(0))
        command.run(billing, input)
      } catch {
        case e: IllegalArgumentException => e.getMessage
      }
      println(res)
    }
  }
}
