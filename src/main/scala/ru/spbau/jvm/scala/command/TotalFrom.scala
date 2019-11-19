package ru.spbau.jvm.scala.command

import java.text.SimpleDateFormat

import ru.spbau.jvm.scala.{ClientsSchema, DB, OperationsSchema, OperationsToClientsSchema, Table}

abstract class TotalFrom(name: String) extends Command(name) {
  val DateParser = new SimpleDateFormat("dd.MM.yyyy")

  protected def allTablesJoint(args: List[String]): Table = {
    val (from, to) = args match {
      case from :: to :: _ =>
        (DateParser.parse(from).getTime, DateParser.parse(to).getTime)
      case from :: _ =>
        (DateParser.parse(from).getTime, Long.MaxValue)
      case _ => (Long.MinValue, Long.MaxValue)
    }

    DB.clientsAndOperationsJoint.
      filterRows(m => from <= m(OperationsSchema.Date).asLong && m(OperationsSchema.Date).asLong <= to)
  }
}
