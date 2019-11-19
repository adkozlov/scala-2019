package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{DB, OperationsSchema}

object Avg extends Command("avg") {
  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left(wrongNumberOfArguments)
    }

    val valuesAsFloats = DB.OperationsTable.getColumn(OperationsSchema.Duration).map(x => x.asFloat)

    Right(valuesAsFloats.iterator.sum / valuesAsFloats.length + "s")
  }

  def help(): String = "avg — average length of call"
}
