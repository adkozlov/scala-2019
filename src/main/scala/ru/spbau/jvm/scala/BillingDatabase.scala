package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date

import ru.spbau.jvm.scala.company_entity.User
import ru.spbau.jvm.scala.operator_entity.Call

import scala.collection.mutable
import scala.io.Source

object BillingDatabase {
  private val DateFormat = new SimpleDateFormat("dd-MM-yyyy")

  type PhoneNumber = String

  private val UserPhoneTableName = "resources/company_entity/user_phone.txt"
  private val UserTableName = "resources/company_entity/users.txt"
  private val CallsTableName = "resources/operator_entity/calls.txt"
  private val ReservedPhonesTableName =
    "resources/operator_entity/reserved_phones.txt"

  private val reservedNumbers = mutable.HashSet[PhoneNumber]()
  private val users = mutable.HashSet[User]()
  private val userPhones = mutable.Map[User, PhoneNumber]()
  private var calls = List[Call]()

  private def loadOperatorDatabase(): Unit = {
    val phoneById = loadPhoneTable()
    loadCallTable(phoneById)
  }

  def loadCompanyDatabase(): Unit = {
    val userById = loadUserTable()
    loadUserPhones(userById)
  }

  private def loadUserTable() = {
    val userById = mutable.HashMap[Int, User]()
    users.clear()
    val usersTable = Source.fromFile(UserTableName)
    for (line <- usersTable.getLines) {
      val splitted = line.split(',')
      val splittedName = splitted(1).split(' ')
      val user = User(splittedName(0), splittedName(1))
      users.add(user)
      userById.put(splitted(0).toInt, user)
    }
    usersTable.close()
    userById
  }

  private def loadUserPhones(userById: mutable.HashMap[Int, User]): Unit = {
    userPhones.clear()
    val userPhoneTable = Source.fromFile(UserPhoneTableName)
    for (line <- userPhoneTable.getLines()) {
      val splitted = line.split(',')
      userPhones.put(userById(splitted(0).toInt), splitted(1))
    }
    userPhoneTable.close()
  }

  private def loadPhoneTable(): mutable.Map[Int, PhoneNumber] = {
    reservedNumbers.clear()
    val phoneById = mutable.HashMap[Int, PhoneNumber]()
    val phonesTable = Source.fromFile(ReservedPhonesTableName)
    for (line <- phonesTable.getLines) {
      val splitted = line.split(',')
      phoneById.put(splitted(0).toInt, splitted(1))
      reservedNumbers.add(splitted(1))
    }
    phonesTable.close()
    phoneById
  }

  private def loadCallTable(phoneById: mutable.Map[Int, PhoneNumber]): Unit = {
    val callsTable = Source.fromFile(CallsTableName)
    calls = callsTable.getLines()
      .map(_.split(','))
      .map(splitted => Call(
        phoneById(splitted(0).toInt),
        DateFormat.parse(splitted(1)),
        splitted(2).toInt,
        splitted(3).toDouble))
      .toList
    callsTable.close()
  }

  def checkState(): Unit = {
    calls.foreach(call => getUserByPhone(call.callee))
  }

  def loadDatabase(): Unit = {
    loadOperatorDatabase()
    loadCompanyDatabase()
    checkState()
  }

  def getAllCalls: List[Call] = calls

  def getAverageDuration: Double =
    calls.map(_.duration).sum / calls.length

  def getPhoneByUser(user: User): Option[PhoneNumber] = {
    if (!users.contains(user)) {
      throw new IllegalArgumentException(s"employee $user not found")
    }
    userPhones.get(user)
  }

  def getUserByPhone(phoneNumber: PhoneNumber): User = {
    if (!reservedNumbers.contains(phoneNumber)) {
      throw new IllegalArgumentException(
        s"Number $phoneNumber is not reserved for the company.")
    }
    val user = userPhones.find(_._2 == phoneNumber)
    if (user.isEmpty) {
      throw new IllegalStateException(
        s"A call has been done from number $phoneNumber, but it is not " +
          "attached to any user.")
    }
    user.get._1
  }

  def getCallsBetweenDates(fromDate: Date, toDate: Date): List[(User, Call)] =
    calls.filter(call =>
      (call.date.after(fromDate) && call.date.before(toDate)) ||
        call.date.equals(fromDate) || call.date.equals(toDate))
    .map(call => (getUserByPhone(call.callee), call))

  def getTotalCostBetweenDates(fromDate: Date, toDate: Date): Double =
    getCallsBetweenDates(fromDate, toDate)
      .map(_._2.cost)
      .sum

  def getUserWithMaxTotalDuration: Option[(User, Double)] =
    getUserWithMaxMetric(_.map(_.duration.toDouble).sum)

  def getUserMaxSpending: Option[(User, Double)] =
    getUserWithMaxMetric(_.map(_.cost).sum)

  private def getUserWithMaxMetric(targetMetric: List[Call] => Double) = {
    val grouped = calls
      .groupBy(_.callee)
      .map { case (number: String, calls: List[Call]) =>
        (getUserByPhone(number), targetMetric(calls)) }

    if (grouped.isEmpty) Option.empty
    else Option(grouped.maxBy(_._2))
  }
}
