package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala._

object CostTotalFrom extends TotalFrom {
  override def execute(args: List[String]): Either[String, String] = {
    val joint: Table = allTablesJoint(args)
    Right(joint.getColumn(OperationsSchema.Duration).
      map(x => x.toFloat * OperationsSchema.FeeDollarsPerSecond).iterator.sum.toString)
  }

  override def help(): String = "total from <dd.mm.yyyy> to <dd.mm.yyyy> — total cost of operations in the given period of time"
}
