package ru.spbau.jvm.scala

import java.nio.file.Paths

object DB {
  private val PathToTables = "resources"
  private val Extension = ".txt"

  private val PathToClients = Paths.get(PathToTables, "clients" + Extension).toString
  private val PathToOperations = Paths.get(PathToTables, "operations" + Extension).toString
  private val PathToOperationsToClients = Paths.get(PathToTables, "operations_to_clients" + Extension).toString

  val ClientsTable = Table.apply(PathToClients, ClientsSchema.Fields)
  val OperationsTable = Table.apply(PathToOperations, OperationsSchema.Fields)
  val OperationsToClientsTable = Table.apply(PathToOperationsToClients, OperationsToClientsSchema.Fields)

  val clientsAndOperationsJoint: Table = DB.OperationsToClientsTable.
    join(DB.OperationsTable, OperationsToClientsSchema.OperationId, OperationsSchema.Id).
    join(DB.ClientsTable, OperationsToClientsSchema.ClientId, ClientsSchema.Id)
}
