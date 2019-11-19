package ru.spbau.jvm.scala.command

object Help extends Command {
  val EndLine = "\n"

  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left("Too many arguments for the command help")
    }

    Right(Command.ActiveCommands.map(_.help()).mkString(EndLine))
  }

  def help(): String = "help — list of available commands"
}
