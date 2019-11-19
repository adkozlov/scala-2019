package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.BillingDB

import scala.collection.mutable.ListBuffer

class NumberCommand extends Command {
  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    if (args.length != 3) {
      return ("Invalid arguments of calls function", List())
    }
    val userIds = db.users
      .filter(user => user.firstName == args(1) && user.lastName == args(2))
      .map(_.id)
    if (userIds.isEmpty) {
      return (s"employee '${args(1)} ${args(2)}' not found", List())
    }
    val userId = userIds.head
    val result = ListBuffer[String]()
    db.phones.filter(_.userId == userId)
      .map(_.phoneNumber)
      .foldLeft(result)(_ += _)
    ("numbers", result)
  }

  override def info(): String = "number VARCHAR VARCHAR -- номер телефона заданного сотрудника"

  override def name(): String = "number"

}
