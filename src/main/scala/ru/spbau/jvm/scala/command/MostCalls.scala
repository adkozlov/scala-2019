package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{ClientsSchema, DB, OperationsSchema, OperationsToClientsSchema}

object MostCalls extends Command("most_calls") {
  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left(wrongNumberOfArguments)
    }

    val client = DB.clientsAndOperationsJoint.groupBy(List(ClientsSchema.FirstName, ClientsSchema.SecondName),
      OperationsSchema.Id).toList.maxBy(_._2.length)._1

    Right(client(0) + " " + client(1))
  }

  def help(): String = "most_calls — client with most amount of calls"
}
