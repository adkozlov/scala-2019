package ru.spbau.jvm.scala.command

abstract class Command(val name: String) {
  def execute(args: List[String]): Either[String, String]
  def help(): String

  protected val wrongNumberOfArguments: String = "wrong number of arguments for command " + name + "."
}

object Command {
  val ActiveCommands: List[Command] = List(CallsTotalFrom, Avg, CostTotalFrom, Number, BiggestTalker, Max, MostCalls, Help)
}
