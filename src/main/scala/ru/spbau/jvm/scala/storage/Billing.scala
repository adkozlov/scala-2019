package ru.spbau.jvm.scala.storage

import java.io.File

import ru.spbau.jvm.scala.storage.db.orm.{Call, Phone, User}

class Billing(val calls: Iterable[Call], val phones: Iterable[Phone], val users: Iterable[User]) {
  def findPhone(phone: String): Option[Phone] = {
    phones.find((it: Phone) => it.phoneNumber == phone)
  }

  def findUserByPhone(phone: Phone): Option[User] = {
    users.find((it: User) => phone.userId == it.id)
  }

  def findUserByPhone(phone: String): Option[User] = {
    val phoneEnt = findPhone(phone)
    if (phoneEnt.isDefined) {
      findUserByPhone(phoneEnt.get)
    } else {
      Option.empty
    }
  }
}

object Billing {
  def load(callPath: File, phonePath: File, userPath: File): Billing = {
    val calls = Call.load(callPath)
    val phones = Phone.load(phonePath)
    val users = User.load(userPath)
    new Billing(calls, phones, users)
  }
}
