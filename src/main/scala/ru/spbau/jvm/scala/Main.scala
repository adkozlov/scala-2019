package ru.spbau.jvm.scala

import java.util.Date

object Main {

  private def help(): Unit = {
    println("calls from DATETIME to DATETIME - список всех звонков за заданный промежуток времени")
    println("avg - средняя длительность звонка")
    println("total from DATETIME to DATETIME - суммарная стоимость услуг связи за заданный промежуток времени")
    println("number VARCHAR VARCHAR - номер телефона заданного сотрудника")
    println("help - вызов справки")
    println("exit - выход")
  }

  private def calls(fromDate: Date, toDate: Date): Unit = {
    println("FirstName | LastName | Caller | Date | Duration (s) | Cost ($)")
    Database.getCalls(fromDate, toDate).foreach(call => printf("%s | %s | %s | %s | %d | %f\n", call.fromUser.firstName, call.fromUser.lastName, call.fromNumber, Database.dateFormat.format(call.date), call.duration, call.cost))
  }

  private def avg(): Unit = {
    printf("%fs\n", Database.getAverageDuration)
  }

  private def total(fromDate: Date, toDate: Date): Unit = {
    printf("$%f\n", Database.getTotalCost(fromDate, toDate))
  }

  private def number(firstName: String, lastName: String): Unit = {
    val number: String = Database.getEmployeeNumber(firstName, lastName)
    number match {
      case null => printf("employee '%s %s' not found\n", firstName, lastName)
      case _    => println(number)
    }
  }

  private def callsBy(firstName: String, lastName: String): Unit = {
    println("Callee | Date | Duration (s) | Cost ($)")
    Database.getEmployeeCalls(firstName, lastName).foreach(call => printf("%s | %s | %d | %f\n", call.toNumber, Database.dateFormat.format(call.date), call.duration, call.cost))
  }

  private def inner(): Unit = {
    println("FirstName | LastName | Caller | FirstName | LastName | Callee | Date | Duration (s) | Cost ($)")
    Database.getInnerCalls.foreach(call => printf("%s | %s | %s | %s | %s | %s | %s | %d | %f\n", call.fromUser.firstName, call.fromUser.lastName, call.fromNumber, call.toUser.firstName, call.toUser.lastName, call.toNumber, Database.dateFormat.format(call.date), call.duration, call.cost))
  }

  private def spender(): Unit = {
    val spender = Database.getSpender
    printf("%s %s $%f\n", spender._1.firstName, spender._1.lastName, spender._2)
  }

  def main(args: Array[String]): Unit = {
    Database.readDatabases()
    val callsPattern = "^calls from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val avgPattern = "^avg$".r
    val totalPattern = "^total from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val numberPattern = "^number ([a-zA-Z']+) ([a-zA-Z']+)$".r
    val callsByPattern = "^calls by ([a-zA-Z']+) ([a-zA-Z']+)$".r
    val innerPattern = "^inner$".r
    val spenderPattern = "^spender$".r
    val helpPattern = "^help$".r
    val exitPattern = "^exit$".r

    while (true) {
      val input = scala.io.StdIn.readLine()
      input match {
        case callsPattern(fromDate, toDate)      => calls(Database.dateFormat.parse(fromDate), Database.dateFormat.parse(toDate))
        case avgPattern()                        => avg()
        case totalPattern(fromDate, toDate)      => total(Database.dateFormat.parse(fromDate), Database.dateFormat.parse(toDate))
        case numberPattern(firstName, lastName)  => number(firstName, lastName)
        case callsByPattern(firstName, lastName) => callsBy(firstName, lastName)
        case innerPattern()                      => inner()
        case spenderPattern()                    => spender()
        case helpPattern()                       => help()
        case exitPattern()                       => return
        case _                                   => printf("command '%s' not found\n", input)
      }
    }
  }
}