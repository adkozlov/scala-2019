package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{DB, FieldValue, OperationsSchema}

object Avg extends Command {
  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left("Too many arguments for the command avg")
    }

    val valuesAsFloats = DB.OperationsTable.getColumn(OperationsSchema.Duration).map(x => x.toFloat)

    Right(valuesAsFloats.iterator.sum / valuesAsFloats.length + "s")
  }

  def help(): String = "avg — average length of call"
}
