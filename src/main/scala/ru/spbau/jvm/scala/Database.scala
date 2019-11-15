package ru.spbau.jvm.scala

import org.joda.time.DateTime
import ru.spbau.jvm.scala.Database.Phone

import scala.collection.mutable
import scala.io.Source

case class User(firstName: String, lastName: String)

case class Call(number: Phone, callee: Phone, date: DateTime, duration: Int, cost: Float)

case class NamedCall(user: User, callee: Phone, duration: Int, cost: Float)

object Database {
  type Phone = String

  val users: mutable.Map[Int, User] = new mutable.HashMap[Int, User]
  val numbers: mutable.Map[Phone, Int] = new mutable.HashMap[Phone, Int]
  val calls = new mutable.HashMap[Phone, mutable.Set[Call]] with mutable.MultiMap[Phone, Call]
  var callsCount = 0

  def loadDatabase(): Unit = {
    val usersTable = Source.fromFile("resources/users.txt")
    usersTable.getLines().foreach { row =>
      val (id, firstName, lastName) = row.split(',') match {
        case Array(id, firstName, lastName) => (id, firstName, lastName)
        case _ => throw new IllegalStateException("Invalid data format in users.txt")
      }

      users.addOne((id.toInt, User(firstName, lastName)))
    }
    usersTable.close()

    val numbersTable = Source.fromFile("resources/numbers.txt")
    numbersTable.getLines().foreach { row =>
      val (userID, number) = row.split(',') match {
        case Array(userID, number) => (userID, number)
        case _ => throw new IllegalStateException("Invalid data format in numbers.txt")
      }

      numbers.addOne((number, userID.toInt))
    }
    numbersTable.close()

    val callsTable = Source.fromFile("resources/calls.txt")
    callsTable.getLines().foreach { row =>
      val (number, callee, time, duration, cost) = row.split(',') match {
        case Array(number, callee, time, duration, cost) => (number, callee, time, duration, cost)
        case _ => throw new IllegalStateException("Invalid data format in calls.txt")
      }

      calls.getOrElseUpdate(number, new mutable.HashSet[Call])
        .addOne(Call(number, callee, DateTime.parse(time), duration.toInt, cost.toFloat))
      callsCount += 1
    }
    calls.values.flatten.map(call => call.duration).sum
    callsTable.close()
  }

  def main(args: Array[String]): Unit = {
    loadDatabase()

    val callsFromToPattern = "^calls from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val avgPattern = "^avg$".r
    val totalFromToPattern = "^total from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val totalFromPattern = "^total from ([0-9:.\\-+T]+)$".r
    val totalPattern = "^total$".r
    val numberPattern = "^number ([a-zA-Z']+) ([a-zA-Z']+)$".r
    val unusedPattern = "^unused$".r
    val uniquePattern = "^unique$".r
    val whoCallsOnThisPattern = "^who-calls-on-this ([+0-9 ]+)$".r
    val helpPattern = "^help$".r
    val exitPattern = "^exit$".r

    while (true) {
      val input = scala.io.StdIn.readLine()

      input match {
        case callsFromToPattern(from, to) =>
          println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
          callList(DateTime.parse(from), DateTime.parse(to)).foreach { call =>
            println(s"${call.user.firstName} | ${call.user.lastName} | ${call.callee} | ${call.duration} | ${Math.round(call.cost * 100.0) / 100.0f}")
          }
        case avgPattern() => println(s"${avg()}s")
        case totalFromToPattern(from, to) => println(s"$$${costs(DateTime.parse(from), DateTime.parse(to))}")
        case totalFromPattern(from) => println(s"$$${costs(DateTime.parse(from))}")
        case totalPattern() => println(s"$$${costs()}")
        case numberPattern(firstName, lastName) => {
          val phone = number(firstName, lastName)
          println(if (phone == null) s"employee '$firstName $lastName' not found" else phone)
        }
        case unusedPattern() => unusedNumbers().foreach(phone => println(phone))
        case uniquePattern() => uniqueCallees().foreach(callee => println(callee))
        case whoCallsOnThisPattern(phone) => whoCallsOnThisNumber(phone).foreach(user => println(s"${user.firstName} ${user.lastName}"))
        case helpPattern() => help()
        case exitPattern() => return
        case _ => println(s"command '$input' not found")
      }
    }
  }

  def callList(from: DateTime = new DateTime(Long.MinValue), to: DateTime = new DateTime(Long.MaxValue)): List[NamedCall] = {
    calls.values.flatten.filter(call => call.date.isAfter(from) && call.date.isBefore(to)).map { call =>
      val number = numbers.find(number => number._1 == call.number)
      val user = if (number.isDefined && users.contains(number.get._2)) {
        users(number.get._2)
      } else User("Unknown", "User")

      NamedCall(user, call.callee, call.duration, call.cost)
    }.toList
  }

  def avg(): Int = Math.ceil(calls.values.flatten.map(call => call.duration).sum.toDouble / callsCount).toInt

  def costs(from: DateTime = new DateTime(Long.MinValue), to: DateTime = new DateTime(Long.MaxValue)): Float =
    Math.round(calls.values.flatten.filter(call => call.date.isAfter(from) && call.date.isBefore(to)).map(call => call.cost).sum * 100.0) / 100.0f

  def number(firstName: String, lastName: String): Phone = {
    val user = users.find(user => user._2.firstName == firstName && user._2.lastName == lastName)

    if (user.isDefined) {
      val number = numbers.find(number => number._2 == user.get._1)
      if (number.isDefined) return number.get._1
    }

    null
  }

  def unusedNumbers(): List[Phone] = numbers.filter(number => number._2 == -1).map(number => number._1).toList

  def uniqueCallees(): Set[Phone] = calls.values.flatten.map(call => call.callee).toSet

  def whoCallsOnThisNumber(number: Phone): Set[User] = callList().filter(call => call.callee == number).map(call => call.user).toSet

  def help(): Unit = {
    println("calls from DATETIME to DATETIME - список всех звонков за заданный промежуток времени")
    println("avg - средняя длительность звонка")
    println("total from DATETIME to DATETIME - суммарная стоимость услуг связи за заданный промежуток времени")
    println("number VARCHAR VARCHAR - номер телефона заданного сотрудника")
    println("unused - незанятые номера из пула")
    println("unique - уникальные номера на которые звонили пользователи")
    println("who-calls-on-this NUMBER - список пользователей звонивших на заданный номер")
    println("help - вызов справки")
    println("exit - выход")
  }
}
