package ru.spbau.jvm.scala

sealed trait Schema

object Schema {
  type Fields = List[(String, String => FieldValue)]
}

object ClientsSchema extends Schema {
  val Id = "ID"
  val FirstName = "First Name"
  val SecondName = "Second Name"

  val Fields: Schema.Fields =
    List((Id, FieldValue.readIntFieldValue),
      (FirstName, FieldValue.readStringFieldValue),
      (SecondName, FieldValue.readStringFieldValue))
}

object OperationsSchema extends Schema {
  val Id = "ID"
  val Callee = "Callee"
  val Caller = "Caller"
  val Duration = "Duration (s)"
  val Date = "Date"

  val FeeDollarsPerSecond: Float = 228

  val Fields: Schema.Fields =
    List((Id, FieldValue.readIntFieldValue),
      (Caller, FieldValue.readStringFieldValue),
      (Callee, FieldValue.readStringFieldValue),
      (Duration, FieldValue.readFloatFieldValue),
      (Date, FieldValue.readLongFieldValue))
}

object OperationsToClientsSchema extends Schema {
  val OperationId = "Operation ID"
  val ClientId = "Client ID"


  val Fields: Schema.Fields =
    List((OperationId, FieldValue.readIntFieldValue),
      (ClientId, FieldValue.readIntFieldValue))
}

