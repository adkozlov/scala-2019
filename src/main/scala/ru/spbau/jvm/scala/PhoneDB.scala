package ru.spbau.jvm.scala

import scala.collection.mutable
import scala.io.Source
import java.time.LocalDate

/**
 * BD client for employees and their phone numbers that supports some SELECT-like REPL operations.
 */
object PhoneDB {
  /**
   * Set of all users ordered by their ids, where id is a serial number in DB.
   */
  private val users = new mutable.HashMap[Int, User]

  /**
   * Set of all company's phones ordered by their ids, where id is a serial number in DB.
   */
  private val reservedPhones = new mutable.HashMap[Int, ReservedPhone]
  private val calls = new mutable.HashSet[Call]()

  /**
   * Stores information that User with id _1 has a phone number _2
   */
  private val userNumberRelation = new mutable.HashMap[Int, Int]()

  /**
   * Split input line into given number of entries by the given delimitier, throws IllegalStateException if
   * number of parameters is wrong (data splited from the internal bd representation).
   */
  private def getDataFromLine(line: String, numberOfParameters: Int, delimiter: Char): Array[String] = {
    if (line == null) {
      return null
    }

    val data = line.split(delimiter)

    if (data.length != numberOfParameters) {
      throw new IllegalStateException("Wrong number of parameters in DB")
    }

    data
  }

  /**
   * Fills users, reservedPhones, calls and userNumberRelation with the data from the BD stored in resources/.
   */
  private def loadDatabase(): Unit = {
    val userSource = Source.fromFile("resources/users.txt")
    userSource.getLines().foreach { line =>
      val data = getDataFromLine(line, 3, ',')

      users.addOne((data.apply(0).toInt, User(data.apply(0).toInt, data.apply(1), data.apply(2))))
    }
    userSource.close()

    val numberSource = Source.fromFile("resources/reservedNumbers.txt")
    numberSource.getLines().foreach { line =>
      val data = getDataFromLine(line, 2, ',')

      reservedPhones.addOne((data.apply(0).toInt, ReservedPhone(data.apply(0).toInt, data.apply(1))))
    }
    numberSource.close()

    val callSource = Source.fromFile("resources/calls.txt")
    callSource.getLines().foreach { line =>
      val data = getDataFromLine(line, 5, ',')
      val yearMonthDate = data.apply(0).split('.').map(s => s.toInt)
      calls.addOne(Call(
        LocalDate.of(yearMonthDate.apply(2), yearMonthDate.apply(1), yearMonthDate.apply(0)),
        Phone(data.apply(1)),
        Phone(data.apply(2)),
        data.apply(3).toInt,
        data.apply(4).toInt
      ))
    }
    callSource.close()

    val userNumberRelationSource = Source.fromFile("resources/usersNumbersRelation.txt")
    userNumberRelationSource.getLines().foreach { line =>
        val data = getDataFromLine(line, 2, ',')
        userNumberRelation += (data.apply(0).toInt -> data.apply(1).toInt)
    }
  }

  /**
   * Split "from DATE1 to DATE2" into (DATE1, DATE2)
   * "from DATE1" into (DATE1, null)
   * "to DATE2" into (null, DATE2)
   * and "" into (null, null)
   */
  private def splitDatesFromLine(line: String): (String, String) = {
    val fromToRegex = "^ from ([0-9:.\\-+T]+) to ([0-9:.\\-+T]+)$".r
    val fromRegex = "^ from ([0-9:.\\-+T]+)$".r
    val toRegex = "^ to ([0-9:.\\-+T]+)$".r
    val emptyRegex = "^$".r

    line match {
      case fromToRegex(from, to) => (from, to)
      case fromRegex(from)       => (from, null)
      case toRegex(to)           => (null, to)
      case emptyRegex()          => (null, null)
      case _                     =>
        println("Wrong date format")
        null
    }
  }

  def main(args: Array[String]): Unit = {
    loadDatabase()

    val callsRegex = "^calls(.*)$".r
    val totalRegex = "^total(.*)$".r
    val avgRegex = "avg(.*)".r
    val numberRegex = "^number ([a-zA-Z']+) ([a-zA-Z']+)$".r
    val helpRegex = "help".r
    val quitRegex = "quit".r

    val userRegex = "^user (.*)$".r
    val allUsersRegex = "^all users$".r
    val callsOfRegex = "^calls of (.*) (.*)$".r

    while (true) {
      val input = scala.io.StdIn.readLine()

      input match {
        case callsOfRegex(firstName, lastName) => printAllCallsOfUser(firstName, lastName)
        case callsRegex(dateRange) => printCallsFromTo(splitDatesFromLine(dateRange))
        case avgRegex(dateRange) => printAvg(splitDatesFromLine(dateRange))
        case totalRegex(dateRange) => printTotal(splitDatesFromLine(dateRange))
        case numberRegex(firstName, lastName) => printPhoneByName(firstName, lastName)
        case helpRegex() => printHelp()
        case quitRegex() => return
        case userRegex(phone) => printNameByPhone(phone)
        case allUsersRegex() => printAllUsers()
        case _ => printInvalidRequestError(input)
      }
    }
  }

  /**
   * Returns true if given date is within given dateRange (inclusive)
   */
  def dateInRange(date: LocalDate, dateRange : (LocalDate, LocalDate)): Boolean = {
    val from = dateRange._1
    val to = dateRange._2

    if (from != null && to != null) {
      date.isAfter(from.minusDays(1)) && date.isBefore(to.plusDays(1))
    } else if (from != null) {
      date.isAfter(from.minusDays(1))
    } else if (to != null) {
      date.isBefore(to.plusDays(1))
    } else {
      true
    }
  }

  def getReservedPhoneById(id: Int) : ReservedPhone = {
    reservedPhones.get(id).orNull
  }

  def getUserById(id: Int) : User = {
    users.get(id).orNull
  }

  def getUserByPhone(phone: Phone) : User = {
    userNumberRelation.foreach(relation => {
      if (getReservedPhoneById(relation._2).phoneNumber.equals(phone.phoneNumber)) {
        return getUserById(relation._1)
      }
    })

    null
  }

  /**
   * Parse ("DATE1", "DATE2") into (DATE1, DATE2)
   */
  def getDateRangeFromString(dateStrings: (String, String)): (LocalDate, LocalDate) = {
    try {
      (getDateFromString(dateStrings._1), getDateFromString(dateStrings._2))
    } catch {
      case _ : IllegalStateException =>
        println("Wrong date format")
        null
    }
  }

  /**
   * Parse "DATE" into DATE
   */
  def getDateFromString(string: String) : LocalDate = {
    if (string == null) {
      return null
    }

    val dataFrom = getDataFromLine(string, 3, '.')
    LocalDate.of(dataFrom.apply(2).toInt, dataFrom.apply(1).toInt, dataFrom.apply(0).toInt)
  }

  /**
   * Print all calls from DATE1 to DATE2
   * If DATE1 is not null and DATE2 is null, prints all calls from DATE1
   * If DATE1 is null and DATE2 is not null, prints all calls before DATE2 (inclusive)
   * If both are nulls, prints all calls in DB.
   */
  def printCallsFromTo(dateRangeString: (String, String)): Unit = {
    if (dateRangeString == null) {
      return
    }

    val dateRange: (LocalDate, LocalDate) = getDateRangeFromString(dateRangeString)

    printCallHeader()

    calls.foreach(call => {
      if (dateInRange(call.date, dateRange)) {
        val user = getUserByPhone(call.caller)
        printCall(user, call)
      }
    })
  }

  /**
   * Prints format of presenting the calls.
   */
  def printCallHeader(): Unit = {
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
  }

  def printCall(user: User, call: Call): Unit = {
    println(user.firstName + " | " + user.lastName + " | " + call.callee + " | " + call.duration + " | " + call.cost)
  }

  /**
   * Prints average length of numbers from DATE1 to DATE2
   * null rules for dates are the same as before
   */
  def printAvg(dateRangeString: (String, String)): Unit = {
    if (dateRangeString == null) {
      return
    }

    val dateRange = getDateRangeFromString(dateRangeString)

    var resultDuration: Double = 0
    var numberOfCalls: Int = 0
    calls.foreach(call => {
      if (dateInRange(call.date, dateRange)) {
        resultDuration += call.duration
        numberOfCalls += 1
      }
    })

    println(Math.ceil(resultDuration / numberOfCalls).toInt + "s")
  }

  /**
   * Prints total cost of numbers from DATE1 to DATE2
   * null rules for dates are the same as before
   */
  def printTotal(dateRangeString: (String, String)): Unit = {
    if (dateRangeString == null) {
      return
    }

    var result: Int = 0

    val dateRange: (LocalDate, LocalDate) = getDateRangeFromString(dateRangeString)

    calls.foreach(call => {
      if (dateInRange(call.date, dateRange)) {
        result += call.cost
      }
    })

    println(result)
  }

  def getPhoneByName(firstName: String, lastName: String): ReservedPhone = {
    userNumberRelation.foreach(relation => {
      val user = getUserById(relation._1)
      if (user.firstName.equals(firstName) && user.lastName.equals(lastName)) {
        return getReservedPhoneById(relation._2)
      }
    })

    null
  }

  def printPhoneByName(firstName: String, lastName: String): Unit = {
    val reservedPhone = getPhoneByName(firstName, lastName)

    if (reservedPhone == null) {
      printEmployeeNotFound(firstName, lastName)
      return
    }

    println(reservedPhone.phoneNumber)
  }

  def printNameByPhone(phone: String): Unit = {
    val user = getUserByPhone(Phone(phone))

    if (user == null) {
      println("phone " + "'" + phone + "' not found")
      return
    }

    println(user.firstName + " " + user.lastName)
  }

  def printHelp(): Unit = {
    println("calls from DATETIME to DATETIME : список всех звонков за заданный промежуток времени")
    println("avg from DATETIME to DATETIME: средняя длительность звонка")
    println("total from DATETIME to DATETIME: суммарная стоимость услуг связи за заданный промежуток времени")
    println("number VARCHAR VARCHAR: номер телефона заданного сотрудника")
    println("help: вызов справки")
    println("quit: выход из программы")
    println("\n")
    println("name VARCHAR: пишет имя сотрудника по номеру телефона")
    println("all users: пишет всех сотрудников и их номера телефонов")
    println("calls of VARCHAR: пишет все вызовы указанного сотрудника")
  }

  def printAllUsers(): Unit = {
    users.values.foreach(user => {
      val phone = getPhoneByName(user.firstName, user.lastName)
      println(user.firstName + " " + user.lastName + " " + phone.phoneNumber)
    })
  }

  def printAllCallsOfUser(firstName : String, lastName : String): Unit = {
    var inputUser: User = null

    users.values.foreach(user => {
      if (user.firstName.equals(firstName) && user.lastName.equals(lastName)) {
        inputUser = user
      }
    })

    if (inputUser == null) {
        printEmployeeNotFound(firstName, lastName)
        return
    }

    val phone = getPhoneByName(firstName, lastName)

    printCallHeader()

    calls.foreach(call => {
      if (call.caller.phoneNumber.equals(phone.phoneNumber)) {
        printCall(inputUser, call)
      }
    })
  }

  def printInvalidRequestError(request: String): Unit = {
    println("command '" + request + "' not found")
  }

  def printEmployeeNotFound(firstName : String, lastName : String): Unit = {
      println("employee " + "'" + firstName + " " + lastName + "' not found")
  }
}

