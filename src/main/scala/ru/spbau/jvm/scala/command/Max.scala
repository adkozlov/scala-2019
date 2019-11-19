package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{DB, OperationsSchema}

object Max extends Command("max") {
  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left(wrongNumberOfArguments)
    }

    Right(DB.OperationsTable.getColumn(OperationsSchema.Duration).map(_.asFloat).max + "s")
  }

  def help(): String = "max — max call duration"
}
