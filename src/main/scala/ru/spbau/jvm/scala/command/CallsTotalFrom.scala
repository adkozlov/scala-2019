package ru.spbau.jvm.scala.command

import java.text.SimpleDateFormat

import ru.spbau.jvm.scala._

object CallsTotalFrom extends TotalFrom {
  val Cost = "Cost (s)"

  override def execute(args: List[String]): Either[String, String]= {
    val joint: Table = allTablesJoint(args)

    Right(joint.addColumn(Cost, joint.getColumn(OperationsSchema.Duration).
      iterator.map(x => FloatValue(x.toFloat * OperationsSchema.FeeDollarsPerSecond)).toList).
      removeColumn(OperationsToClientsSchema.OperationId).removeColumn(OperationsSchema.Id).
      removeColumn(OperationsToClientsSchema.ClientId).removeColumn(ClientsSchema.Id).
      removeColumn(OperationsSchema.Caller).removeColumn(OperationsSchema.Date).print())

  }

  override def help(): String = "calls from <dd.mm.yyyy> to <dd.mm.yyyy> — display all calls made in the given period of time"
}
