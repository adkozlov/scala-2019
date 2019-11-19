package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{ClientsSchema, DB, OperationsSchema, OperationsToClientsSchema}

object BiggestTalker extends Command("biggest_talker") {
  override def execute(args: List[String]): Either[String, String] = {
    if (args.nonEmpty) {
      return Left(wrongNumberOfArguments)
    }

    val client = DB.clientsAndOperationsJoint.groupBy(List(ClientsSchema.FirstName, ClientsSchema.SecondName),
      OperationsSchema.Duration).toList.maxBy(_._2.map(_.asFloat).sum)._1

    Right(client(0) + " " + client(1))
  }

  def help(): String = "biggest_talker — client with most call time"
}
