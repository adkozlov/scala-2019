package ru.spbau.jvm.scala

import org.joda.time.DateTime

object Main {
  val database = new Database()

  def main(args: Array[String]): Unit = {
    database.loadDatabase("resources/users.csv", "resources/calls.csv")

    val callsFromToPattern = "^calls from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val avgPattern = "^avg$".r
    val totalFromToPattern = "^total from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val numberPattern = "^number ([ a-zA-Z']+)$".r
    val helpPattern = "^help$".r

    val callsFromNamePattern = "^calls from ([ a-zA-Z']+)$".r
    val callsToNumberPattern = "^calls to ([ +0-9']+)$".r
    val totalFromNamePattern = "^total from ([ a-zA-Z']+)$".r
    val exitPattern = "^exit$".r

    while (true) {
      val input = scala.io.StdIn.readLine()

      input match {
        case callsFromToPattern(from, to) =>
          println("Name | Callee | Duration (s) | Cost ($)")
          getCallsByPeriod(
            DateTime.parse(from, Database.formatter),
            DateTime.parse(to, Database.formatter)
          ).foreach { call =>
            println(
              s"${call.callerNumber.name} | ${call.calleePhoneNumber} | ${call.duration} | ${call.cost}"
            )
          }
        case avgPattern() => println(s"${avg()}s")
        case totalFromToPattern(from, to) =>
          println(
            s"$$${getTotalByPeriod(DateTime.parse(from, Database.formatter), DateTime.parse(to, Database.formatter))}"
          )
        case numberPattern(name) => println(s"${getNumberFromName(name)}")
        case callsFromNamePattern(name) =>
          println("Callee | Date | Duration (s) | Cost ($)")
          getCallsFromName(name).foreach { call =>
            println(s"${call.calleePhoneNumber} | ${call.date
              .formatted("dd.MM.yyyy")} | ${call.duration} | ${call.cost}")
          }
        case callsToNumberPattern(number) =>
          println("Name | Date | Duration (s) | Cost ($)")
          getCallsToNumber(number).foreach { call =>
            println(s"${call.callerNumber.name} | ${call.date
              .formatted("dd.MM.yyyy")} | ${call.duration} | ${call.cost}")
          }
        case totalFromNamePattern(name) =>
          println(s"$$${getTotalFromName(name)}")
        case helpPattern() => help()
        case exitPattern() => return
        case _             => println(s"command '$input' not found")
      }
    }
  }

  def getCallsByPeriod(from: DateTime, to: DateTime): List[Call] =
    database.callsList
      .filter(
        call => call.date.compareTo(from) >= 0 && call.date.compareTo(to) <= 0
      )
      .toList

  def getCallsFromName(name: String): List[Call] =
    database.callsList
      .filter(call => call.callerNumber.name.equals(name))
      .toList

  def getCallsToNumber(number: String): List[Call] =
    database.callsList
      .filter(call => call.calleePhoneNumber.equals(number))
      .toList

  def getTotalByPeriod(from: DateTime, to: DateTime): Double =
    getCallsByPeriod(from, to).map(call => call.cost).sum

  def avg(): Double =
    database.callsList
      .map(call => call.duration)
      .sum
      .toDouble / database.callsList.length

  def getNumberFromName(name: String): String =
    database.nameToNumber.getOrElse(name, "There is no such man")

  def getTotalFromName(name: String): Double =
    database.callsList
      .filter(call => call.callerNumber.name.equals(name))
      .map(call => call.cost)
      .sum

  def help(): Unit = {
    println(
      "calls from DATETIME to DATETIME - список всех звонков за заданный промежуток времени"
    )
    println("avg - средняя длительность звонка")
    println(
      "total from DATETIME to DATETIME - суммарная стоимость услуг связи за заданный промежуток времени"
    )
    println("number NAME - номер телефона заданного сотрудника")
    println("calls from NAME - все звонки сотрудника")
    println("calls to NUMBER - все звонки на номер")
    println(
      "total from NAME - суммарная стоимость услуг связи за одного сотрудника за все время"
    )
    println("help - вызов справки")
    println("exit - выход")
  }
}
