package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable
import scala.io.Source

object Database {

  private val userTablePath = "resources/numbers_owners.txt"
  private val providedNumbersTablePath = "resources/provided_numbers.txt"
  private val callTablePath = "resources/calls.txt"

  private val users = mutable.HashSet[User]()
  private val calls = mutable.HashSet[Call]()
  private val providedNumbers = mutable.HashMap[Int, String]()
  private val numbersToUsers = mutable.HashMap[Int, User]()

  def load(): Unit = {
    try {
      loadNumbers()
      loadUsers()
      loadCalls()
    } catch {
      case _: NumberFormatException => println("Tables' format is not correct")
    }
  }

  def getCallsWithCostMoreOrEqualTo(cost: Double): List[Call] = {
    calls.filter(_.cost >= cost).toList
  }

  def getCallsWithDurationMoreOrEqualTo(duration: Int): List[Call] = {
    calls.filter(_.duration >= duration).toList
  }

  def getNumberByName(firstName: String, lastName: String): Option[String] = {
    users.find(u => u.firstName == firstName && u.lastName == lastName).map(_.number)
  }

  def getAverageDuration: Double = {
    val sum = calls.foldLeft(0)((res, call) => res + call.duration)
    sum.toDouble / calls.size
  }

  def getCallsBetweenDates(date1: Date, date2: Date): List[Call] = {
    calls.filter(_.date.compareTo(date1) >= 0).filter(_.date.compareTo(date2) <= 0).toList
  }

  def getTotalBetweenDates(date1: Date, date2: Date): Double = {
    calls.filter(_.date.compareTo(date1) >= 0).filter(_.date.compareTo(date2) <= 0)
      .foldLeft(0.0)((total, cur) => total + cur.cost)
  }

  def getMaxSpender: User = {
    calls.foldLeft(mutable.Map[User, Double]().withDefaultValue(0)) { (m, c) => {
      m(c.user) += c.cost
      m
    }
    }.maxBy(_._2)._1
  }

  private def loadNumbers(): Unit = {
    val numbersTable = Source.fromFile(providedNumbersTablePath)
    for (line <- numbersTable.getLines) {
      val splittedLine = line.split(",")
      val id = splittedLine(0).toInt
      val number = splittedLine(1)
      providedNumbers.put(id, number)
    }
    numbersTable.close
  }

  private def loadUsers(): Unit = {
    val userTable = Source.fromFile(userTablePath)
    for (line <- userTable.getLines) {
      val splittedLine = line.split(",")
      val name = splittedLine(0).split(" ")
      val firstName = name(0)
      val lastName = name(1)
      val numberId = splittedLine(1).toInt
      val number = providedNumbers.getOrElse(numberId, null)
      val user = User(firstName, lastName, number)
      users.add(user)
      numbersToUsers.put(numberId, user)
    }
    userTable.close
  }

  private def loadCalls(): Unit = {
    val callsTable = Source.fromFile(callTablePath)
    for (line <- callsTable.getLines()) {
      val splittedLine = line.split(",")
      val numberId = splittedLine(0).toInt
      val callee = splittedLine(1)
      val duration = splittedLine(2).toInt
      val cost = splittedLine(3).toDouble
      val date = new SimpleDateFormat("dd.MM.yyyy").parse(splittedLine(4))
      val user = numbersToUsers.get(numberId).orNull
      calls.add(Call(user, callee, duration, cost, date))
    }
    callsTable.close
  }
}
