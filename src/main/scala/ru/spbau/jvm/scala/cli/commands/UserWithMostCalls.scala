package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.BillingDB

class UserWithMostCalls extends Command {
  override def execute(db: BillingDB, args: Array[String]): (String, Iterable[String]) = {
    val callsWithPhones = db.calls
      .map(call => (call, db.phones.find(phone => phone.phoneNumber == call.phoneNumberFrom).get))
    val userId = callsWithPhones
      .map(
        callUser => (callUser._1, callUser._2, db.users.find(user => user.id == callUser._2.userId)
          .get)
      ).groupBy(a => a._2.id).map(a => (a._1, a._2.size)).maxBy(a => a._2)
    if (userId == null) {
      return ("FirstName | LastName | CallsCount", List())
    }
    val user = db.users.find(u => u.id == userId._1).get
    ("FirstName | LastName | CallsCount", List(List(user.firstName, user.lastName, userId._2).mkString(" | ")))
  }

  override def info(): String =
    "userMostCalls -- пользователь с самым большим количеством звонков"

  override def name(): String = "userMostCalls"

}
