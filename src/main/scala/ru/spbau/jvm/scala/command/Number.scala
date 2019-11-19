package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.{ClientsSchema, DB}

object Number extends Command {
  override def execute(args: List[String]): Either[String, String] = {
    val nameSurnameE = args match {
      case name :: surname :: _ => Right((name, surname))
      case _ => Left("Wrong number of argu,ents for the command number")
    }

    if (nameSurnameE.isLeft) {
      val Left(e) = nameSurnameE
      return Left(e)
    }

    val Right((name, surname)) = nameSurnameE

    Right(DB.ClientsTable.filterRows(x => x(ClientsSchema.FirstName).toString == name
      && x(ClientsSchema.SecondName).toString == surname).print())
  }

  def help(): String = "avg — average length of call"
}
