package ru.spbau.jvm.scala.command

import ru.spbau.jvm.scala.storage.Billing

object Employee extends Command {
  override val name: String = "employee"

  override val help: String = "find employee by number"

  override def run(storage: Billing, args: Array[String]): String = {
    val user = storage.findUserByPhone(args(1))
    if (user.isEmpty) {
      return s"Cannot find phone ${args(1)}"
    }
    s"${user.get.firstName} ${user.get.lastName}"
  }
}
