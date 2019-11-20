package ru.spbau.jvm.scala

import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer

class Database(private val usersTableFilename: String = "resources/Users.txt",
               private val phoneCallsTableFilename: String = "resources/PhoneCalls.txt")
{

  private val phoneCallsTable = new PhoneCallsTable()
  private var calls = List[PhoneCall]()
  private val usersTable= new UsersTable()
  private var users = List[User]()

  def load(): Unit = {
    calls = phoneCallsTable.load(phoneCallsTableFilename)
    users = usersTable.load(usersTableFilename)
  }

  private def leftJoin(calls: List[PhoneCall], users: List[User]): List[(PhoneCall, Option[User])] = {
    calls.map(call => (call, users.find(user => user.phone.equals(call.phone))))
  }

  private def rightJoin(calls: List[PhoneCall], users: List[User]): List[(Option[PhoneCall], User)] = {
    users.map(user => (calls.find(call => call.phone.equals(user.phone)), user))
  }

  def getCallsInPeriod(from: DateTime = new DateTime(Long.MinValue),
                       to: DateTime = new DateTime(Long.MaxValue)
                      ): List[(String, String, String, Int, Double)] = {
    val callsInPeriod = calls.filter(call => call.date.isAfter(from) && call.date.isBefore(to))
    val result = ListBuffer.empty[(String, String, String, Int, Double)]
    for ((call, optionUser) <- leftJoin(callsInPeriod, users)) {
      if (optionUser.isEmpty) {
        result.addOne(("NULL", "NULL", call.phone, call.duration, call.cost))
      } else {
        val user = optionUser.get
        result.addOne((user.name, user.surname, call.phone, call.duration, call.cost))
      }
    }
    result.toList
  }

  def getCallsAverageDuration: Double = {
    if (calls.isEmpty) return 0
    calls.map(call => call.duration).sum.toDouble / calls.size
  }

  def getMaxDuration: Option[Double] = {
    val duration = calls.map(call => call.duration)
    if (duration.isEmpty) {
      return Option.empty
    }
    Option(duration.max)
  }

  def getMinDuration: Option[Double] = {
    val duration = calls.map(call => call.duration)
    if (duration.isEmpty) {
      return Option.empty
    }
    Option(duration.min)
  }

  def getCostInPeriod(from: DateTime = new DateTime(Long.MinValue),
                      to: DateTime = new DateTime(Long.MaxValue)): Double = {
    calls.filter(call => call.date.isAfter(from) && call.date.isBefore(to))
      .map(call => call.duration)
      .sum.toDouble
  }

  def getPhoneFromUser(name: String, surname: String): Option[String] = {
    users.find(user => user.name.equals(name) && user.surname.equals(surname))
      .fold[Option[String]] {
        Option.empty
      } { user =>
        Option(user.phone)
      }
  }

  def getUserFromPhone(phone: String): Option[(String, String)] = {
    users.find(user => user.phone.equals(phone))
      .fold[Option[(String, String)]] {
        Option.empty
      } { user =>
        Option((user.name, user.surname))
      }
  }
}