package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date

import Patterns._

import scala.collection.immutable.HashMap
import scala.io.{Source, StdIn}
import scala.util.control.Breaks.break

case class Employee(number: String, name: String, surname: String)

case class Call(callerNumber: String, calleeNumber: String, date: Date, duration: Int, cost: Double)

object Main {

  private val format = new SimpleDateFormat("dd.MM.yyyy")

  private var employees: Array[Employee] = new Array(0)
  private var namesByNumbers: Map[String, (String, String)] = new HashMap()
  private var numbersByNames: Map[(String, String), String] = new HashMap()
  private var calls: Array[Call] = new Array(0)

  private def dateFilter(from: Date, to: Date) = { call: Call =>
    call.date.getTime >= from.getTime && call.date.getTime <= to.getTime
  }

  def loadCalls(): Unit = {
    val input = Source.fromFile("resources/calls.txt")
    calls = input.getLines().map(line => {
      val tokens = line.split(",")
      Call(tokens(0), tokens(1), format.parse(tokens(2)), tokens(3).toInt, tokens(4).toDouble)
    }).toArray
  }

  def loadEmployees(): Unit = {
    val input = Source.fromFile("resources/employees.txt")
    employees = input.getLines().map(line => {
      val tokens = line.split(",")
      Employee(tokens(0), tokens(1), tokens(2))
    }).toArray
    namesByNumbers = employees.map(it => (it.number, (it.name, it.surname))).toMap
    numbersByNames = employees.map(it => ((it.name, it.surname), it.number)).toMap
  }

  def getCalls(from: Date, to: Date): Array[Call] = {
    calls.filter(dateFilter(from, to))
  }

  def getAvg: Double = {
    calls.map(it => it.duration).sum.toDouble / calls.length
  }

  def getTotal(from: Date = format.parse("01.01.0000"), to: Date = format.parse("31.12.9999")): Double = {
    calls.filter(dateFilter(from, to)).map(it => it.cost).sum
  }

  def getTotalBy(name: String, surname: String): Double = {
    calls.filter(call => call.callerNumber == numbersByNames((name, surname))).map(it => it.cost).sum
  }

  def getMaxTotal: String = {
    val (name, surname) = employees.map(it => (it.name, it.surname)).maxBy(it => getTotalBy(it._1, it._2))
    s"$name $surname"
  }

  def getNumber(name: String, surname: String): String = {
    numbersByNames.getOrElse((name, surname), s"employee '$name $surname' not found")
  }

  def getName(number: String): String = {
    if (namesByNumbers.contains(number)) {
      val (name, surname) = namesByNumbers(number)
      s"$name $surname"
    } else {
      s"number '$number' not found"
    }
  }

  def printHelp(): Unit = {
    println(
      """BILLING TOOL
        | calls from DATETIME to DATETIME -- список всех звонков за заданный промежуток времени
        | avg -- средняя длительность звонка
        | total [from DATETIME [to DATETIME]] -- суммарная стоимость услуг связи за заданный промежуток времени
        | total by NAME SURNAME -- суммарная стоимость звонков заданного сотрудника
        | max total -- имя сотрудника с наибольшей суммарной стоимостью звонков
        | number NAME SURNAME -- номер телефона заданного сотрудника
        | name NUMBER -- имя сотрудника, использующего данный телефон
        | help -- вызов справки
        | exit -- выйти""".stripMargin
    )
  }

  def main(args: Array[String]): Unit = {
    loadEmployees()
    loadCalls()

    while (true) {
      val command = StdIn.readLine()

      command match {
        case calls(from, to) =>
          println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
          getCalls(format.parse(from), format.parse(to)).foreach(call => {
            val (name, surname) = namesByNumbers(call.callerNumber)
            println(s"$name | $surname | ${call.calleeNumber} | ${call.duration} | ${call.cost}")
          })
        case avg() => println(s"${getAvg}s")
        case totalFromTo(from, to) => println(getTotal(format.parse(from), format.parse(to)))
        case totalFrom(from) => println(getTotal(format.parse(from)))
        case total() => println(getTotal())
        case totalBy(name, surname) => println(getTotalBy(name, surname))
        case maxTotal() => println(getMaxTotal)
        case number(name, surname) => println(getNumber(name, surname))
        case name(number) => println(getName(number))
        case help() => printHelp()
        case exit() => break
        case _ => println(s"command '$command' not found")
      }
    }
  }
}