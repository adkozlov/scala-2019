package ru.spbau.jvm.scala

sealed class Schema(val name: String) {
  private val Dot = "."

  def withName(s: String): String = name + Dot + s
}

object Schema {
  type Fields = List[(String, String => FieldValue)]
}

object ClientsSchema extends Schema("Clients") {
  val Id: String = withName("ID")
  val FirstName: String = withName("FirstName")
  val SecondName: String = withName("SecondName")
  val Number: String = withName("Number")

  val Fields: Schema.Fields =
    List((Id, FieldValue.readIntFieldValue),
      (FirstName, FieldValue.readStringFieldValue),
      (SecondName, FieldValue.readStringFieldValue),
      (Number, FieldValue.readStringFieldValue))
}

object OperationsSchema extends Schema("Operations") {
  val Id: String = withName("ID")
  val Callee: String = withName("Callee")
  val Duration: String = withName("Duration")
  val Date: String = withName("Date")

  val FeeDollarsPerSecond: Float = 228

  val Fields: Schema.Fields =
    List((Id, FieldValue.readIntFieldValue),
      (Callee, FieldValue.readStringFieldValue),
      (Duration, FieldValue.readFloatFieldValue),
      (Date, FieldValue.readLongFieldValue))
}

object OperationsToClientsSchema extends Schema("OperationsToClients") {
  val OperationId: String = withName("OperationID")
  val ClientId: String = withName("ClientID")

  val Fields: Schema.Fields =
    List((OperationId, FieldValue.readIntFieldValue),
      (ClientId, FieldValue.readIntFieldValue))
}

