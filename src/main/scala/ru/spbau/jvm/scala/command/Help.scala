package ru.spbau.jvm.scala.command

object Help extends Command("help") {
  val EndLine = "\n"

  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left(wrongNumberOfArguments)
    }

    Right(Command.ActiveCommands.map(_.help()).mkString(EndLine))
  }

  def help(): String = "help — list of available commands"
}
