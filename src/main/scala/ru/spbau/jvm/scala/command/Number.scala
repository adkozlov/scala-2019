package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.orm.{Phone, User}

object Number extends Command {
  override val name: String = "number"

  override val help: String = "find number by employee"

  override def run(storage: Billing, args: Array[String]): String = {
    val idOpt = storage.users.find((it: User) => it.firstName == args(1) && it.lastName == args(2))
    if (idOpt.isEmpty) {
      return s"Employee ${args(1)} ${args(2)} not found"
    }
    val phone = storage.phones.find((it: Phone) => it.userId == idOpt.get.id).getOrElse(throw new IllegalArgumentException("Cannot find phone")).phoneNumber
    phone
  }
}

