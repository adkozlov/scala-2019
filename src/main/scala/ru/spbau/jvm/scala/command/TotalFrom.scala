package ru.spbau.jvm.scala.command

import java.text.SimpleDateFormat

import ru.spbau.jvm.scala.{ClientsSchema, DB, OperationsSchema, OperationsToClientsSchema, Table}

abstract class TotalFrom extends Command {
  val DateParser = new SimpleDateFormat("dd.mm.yyyy")

  protected def allTablesJoint(args: List[String]): Table = {
    val (from, to) = args match {
      case from :: to :: _ =>
        (DateParser.parse(from).getTime, DateParser.parse(to).getTime)
      case from :: _ => (DateParser.parse(from).getTime, Long.MaxValue)
      case _ => (Long.MinValue, Long.MaxValue)
    }

    DB.OperationsToClientsTable.
      join(DB.OperationsTable, OperationsToClientsSchema.OperationId, OperationsSchema.Id).
      join(DB.ClientsTable, OperationsToClientsSchema.ClientId, ClientsSchema.Id).
      filterRows(m => from <= m(OperationsSchema.Date).toLong && m(OperationsSchema.Date).toLong <= to)
  }
}
