package ru.spbau.jvm.scala.cli.commands

import org.scalatest.{FlatSpec, Matchers}

class HelpCommandSpec extends FlatSpec with Matchers {
  val avgCommand = new AvgCommand
  val commands = List(avgCommand)
  val helpCommand = new HelpCommand(commands)

  "Help command name" should "be \"help\"" in {
    helpCommand.name should be("help")
  }

  "Help command info" should "be \"help -- вызов справки\"" in {
    helpCommand.info should be("help -- вызов справки")
  }

  "Help command" should "return info of commands" in {
    helpCommand.execute(null, Array("help")) should be(("help", List(avgCommand.info())))
  }
}
