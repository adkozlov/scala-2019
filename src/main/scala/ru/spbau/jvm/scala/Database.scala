package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.TimeUnit

import scala.collection.immutable
import scala.io.Source

case class User(phone: String, firstName: String, lastName: String)

case class Call(fromNumber: String, toNumber: String, fromUser: User, toUser:User, date: Date, duration: Int, cost: Double)

object Database {
  private var users: Array[User] = new Array[User](0)
  private var userFromNumber: immutable.Map[String, User] = new immutable.HashMap[String, User]()
  private var numberFromUser: immutable.Map[(String, String), String] = new immutable.HashMap[(String, String), String]()
  private var calls: Array[Call] = new Array[Call](0)

  val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def readDatabases(): Unit = {
    val usersTable = Source.fromFile("resources/users.txt")
    users = usersTable.getLines().map(line => {
      val (phone, firstName, lastName) = line.split(";") match {
        case Array(phone, firstName, lastName) => (phone, firstName, lastName)
        case _ => throw new IllegalArgumentException("Incorrect file format 'users.xt'")
      }
      User.apply(phone, firstName, lastName)
    }).toArray

    userFromNumber = users.map(user => (user.phone, user)).toMap
    numberFromUser = users.map(user => ((user.firstName, user.lastName), user.phone)).toMap

    val callsTable = Source.fromFile("resources/calls.txt")
    calls = callsTable.getLines().map(line => {
      val (from, to, date, duration, cost) = line.split(";") match {
        case Array(from, to, date, duration, cost) => (from, to, date, duration, cost)
        case _ => throw new IllegalArgumentException("Incorrect file format 'calls.xt'")
      }
      Call.apply(from, to, userFromNumber.getOrElse(from, null), userFromNumber.getOrElse(to, null), dateTimeFormat.parse(date), duration.toInt, cost.toDouble)
    }).toArray
  }

  private def filterDate(fromDate: Date, toDate: Date) = {
    (call: Call) => call.date.after(fromDate) && call.date.before(new Date(toDate.getTime +  TimeUnit.DAYS.toMillis(1)))
  }

  def getCalls(fromDate: Date, toDate: Date): Array[Call] =
    calls.filter(filterDate(fromDate, toDate))

  def getAverageDuration: Double =
    calls.map(_.duration).sum.toDouble / calls.length

  def getTotalCost(fromDate: Date, toDate: Date): Double =
    calls.filter(filterDate(fromDate, toDate)).map(_.cost).sum

  def getEmployeeNumber(firstName: String, lastName: String): String =
    numberFromUser.getOrElse((firstName, lastName), null)

  def getEmployeeCalls(firstName: String, lastName: String): Array[Call] =
    calls.filter(call => call.fromUser != null && call.fromUser.firstName == firstName && call.fromUser.lastName == lastName)

  def getInnerCalls: Array[Call] =
    calls.filter(call => call.fromUser != null && call.toUser != null)

  def getSpender: (User, Double) =
    calls.groupBy(_.fromUser).map({case (user: User, calls: Array[Call]) => (user, calls.map(_.cost).sum) }).toArray.maxBy(_._2)
}
