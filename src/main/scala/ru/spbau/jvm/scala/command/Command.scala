package ru.spbau.jvm.scala.command

trait Command {
  def execute(args: List[String]): Either[String, String]
  def help(): String
}

object Command {
  val ActiveCommands: List[Command] = List(CallsTotalFrom, Avg, CostTotalFrom, Number, Help)
}
