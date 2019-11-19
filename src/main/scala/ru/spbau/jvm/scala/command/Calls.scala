package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.orm.{Call, User}
import ru.spbau.jvm.scala.util.Interval

object Calls extends Command {
  override val name: String = "calls"

  override val help: String = "list of calls in database"

  private case class Pair(user: Option[User], call: Call)

  override def run(storage: Billing, args: Array[String]): String = {
    val (from, to) = Interval.parse(args.drop(1))

    Interval.filterCalls(from, to, storage.calls)
      .map((it: Call) => Pair(storage.findUserByPhone(it.from), it))
      .map((pair: Pair) => if (pair.user.isDefined) {
        s"${pair.user.get.firstName} | ${pair.user.get.lastName} | ${pair.call.from} | ${pair.call.duration} | ${pair.call.cost}"
      } else {
        ""
      }).filter(!_.isBlank).mkString("\n")
  }
}
