package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{ClientsSchema, DB}

object Number extends Command("number") {
  override def execute(args: List[String]): Either[String, String] = {
    val nameSurnameE = args match {
      case name :: surname :: _ => Right((name, surname))
      case _ => Left("Wrong number of arguments for the command number")
    }

    if (nameSurnameE.isLeft) {
      val Left(e) = nameSurnameE
      return Left(e)
    }

    val Right((name, surname)) = nameSurnameE

    Right(DB.ClientsTable.filterRows(x => x(ClientsSchema.FirstName).asString == name
      && x(ClientsSchema.SecondName).asString == surname).getColumn(ClientsSchema.Number).head.toString)
  }

  def help(): String = "number VARCHAR VARCHAR — gets phone number of given client"
}
