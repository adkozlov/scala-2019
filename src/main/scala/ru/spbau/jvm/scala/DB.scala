package ru.spbau.jvm.scala

object DB {
  private val PathToTables = "resources"
  private val Extension = ".txt"

  private val PathToClients = PathToTables + "clients" + Extension
  private val PathToOperations = PathToTables + "operations" + Extension
  private val PathToOperationsToClients = PathToTables + "operations_to_clients" + Extension

  val ClientsTable = Table.apply(PathToClients, ClientsSchema.Fields)
  val OperationsTable = Table.apply(PathToOperations, OperationsSchema.Fields)
  val OperationsToClientsTable = Table.apply(PathToOperationsToClients, OperationsToClientsSchema.Fields)
}
