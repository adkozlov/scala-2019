package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala._

object CostTotalFrom extends TotalFrom("total") {
  override def execute(args: List[String]): Either[String, String] = {
    val joint: Table = allTablesJoint(args)
    Right(joint.getColumn(OperationsSchema.Duration).
      map(x => x.asFloat * OperationsSchema.FeeDollarsPerSecond).iterator.sum.toString)
  }

  override def help(): String = "total from DATETIME to DATETIME — total cost of calls in the given period of time"
}
