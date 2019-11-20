package ru.spbau.jvm.scala

import java.time.format.DateTimeParseException
import java.time.{LocalDateTime, ZoneOffset}

import ru.spbau.jvm.scala.CallsParser.{ExitParser, HelpParser, NotFoundParser}

import scala.collection.mutable

class CommandParser {

  val helpString: String = "Commands:\n" +
    "call:\n"

  def parsers: List[CommandParser] = List(CallsParser,
    AvgParser,
    TotalParser,
    NumberParser,
    LongestParser,
    PersonnelParser,
    ConnectionsParser,
    HelpParser,
    ExitParser)

  def help: String = ""

  def parseCommand(command: String): Boolean = {
    val words = command.split(" ").toList
    val commandName = words(0)
    val parser = commandName match {
      case "calls" => CallsParser
      case "avg" => AvgParser
      case "total" => TotalParser
      case "number" => NumberParser
      case "longest" => LongestParser
      case "personnel" => PersonnelParser
      case "connections" => ConnectionsParser
      case "help" => HelpParser
      case "exit" => ExitParser
      case _ => NotFoundParser
    }
    parser.parseCommand(words)
  }

  def parseCommand(command: List[String]): Boolean = {
    parseCommand(command.mkString(" "))
  }

}

object ConnectionsParser extends CommandParser {

  override def help: String = "connections \ncount unique outgoing phone numbers for each phone number"

  override def parseCommand(command: List[String]): Boolean = {
    val connections = new mutable.HashMap[String, mutable.HashSet[String]]
      //.withDefault(_ => mutable.HashSet[String]())
    println("Phone number | connections number")
    CallTable().all.foreach(call => {
      if (!connections.contains(call.callerNumber)) {
        connections.put(call.callerNumber, mutable.HashSet[String]())
      }
      connections(call.callerNumber).add(call.calleeNumber)
    })
    connections.map(pr => (pr._1, pr._2.size))
      .foreach(pr => println(pr._1 + " | " + pr._2))
    true
  }
}

object PersonnelParser extends CommandParser {

  override def help: String = "personnel \nget overall outgoing call duration for each user"

  override def parseCommand(command: List[String]): Boolean = {
    val durations = new mutable.HashMap[String, Long].withDefaultValue(0)
    val users = UserTable()
    CallTable().all.foreach(call => {
      durations(call.callerNumber) = durations(call.callerNumber) + call.duration.getSeconds
    }
    )
    durations.foreach(pr => {
      var firstName = pr._1
      var lastName = ""
      val usersWithPhoneNumber = users.usersByPhoneNumber(pr._1)
      if (usersWithPhoneNumber.nonEmpty) {
        firstName = usersWithPhoneNumber(0).firstName
        lastName = usersWithPhoneNumber(0).lastName
      }
      println(firstName + " " + lastName + " " + pr._2)
    }
    )
    true
  }
}

object LongestParser extends CommandParser {

  override def help: String = "longest \nshow the longest call"

  override def parseCommand(command: List[String]): Boolean = {
    val calls = CallTable()
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
    val longestCall = calls.all.maxBy(calls => calls.duration.getSeconds)
    println(longestCall)
    true
  }
}

object NumberParser extends CommandParser {

  override def help: String =
    "number <First name> <Last name> \nget phone number of certain emplyee"

  override def parseCommand(command: List[String]): true = {
    if (command.length < 3) {
      println("Wrong query format")
      return true
    }
    val firstName = command(1)
    val lastName = command(2)
    val userRecords = UserTable().all
      .filter(user => user.firstName == firstName && user.lastName == lastName)
    if (userRecords.isEmpty) {
      println(s"employee $firstName $lastName not found")
    } else {
      userRecords.map(user => user.phoneNumber)
        .foreach(println)
    }
    true
  }
}

object TotalParser extends CommandParser {

  override def help: String = "total from <from DATETIME> to <to DATETIME> \nget overall cost for certain time interval"

  override def parseCommand(command: List[String]): Boolean = {
    def printTotalSum(calls: List[Call]): Unit = {
      val sumInCents = calls.map(call => call.cost.cents).sum
      println(new Money(sumInCents))
    }

    if (command.length > 4) {
      val from: String = command(2)
      val to: String = command(4)
      try {
        val fromTime = LocalDateTime.parse(from)
        val toTime = LocalDateTime.parse(to)
        val calls = CallTable().getCallsTimeInterval(fromTime, toTime)
        printTotalSum(calls)
      } catch {
        case _: DateTimeParseException =>
          println("Wrong query format")
          return true
      }

    } else if (command.length > 2) {
      val time = LocalDateTime.parse(command(2))
      if (command(1) == "from") {
        val calls = CallTable().getCallsFromTime(time)
        printTotalSum(calls)
      } else if (command(1) == "to") {
        val calls = CallTable().getCallsBeforeTime(time)
        printTotalSum(calls)
      } else {
        println("Wrong query format")
      }
    } else {
      println("Wrong query format")
    }
    true
  }
}

object AvgParser extends CommandParser {

  override def help: String = "avg \nget average call duration"

  override def parseCommand(command: List[String]): Boolean = {
    val calls = CallTable()
    val timeValues = calls.all.map(call => call.duration.getSeconds)
    val avg = if (timeValues.isEmpty) {
      0
    } else {
      timeValues.sum / timeValues.length
    }
    println(avg + "s")
    true
  }
}

object CallsParser extends CommandParser {


  override def help: String =
    "calls from <yyyy-mm-ddT:hh:mm:ss> to <yyyy-mm-ddT:hh:mm:ss> \nget all calls for time interval"

  override def parseCommand(command: List[String]): Boolean = {
    val users = UserTable()
    if (command.length < 5) {
      println("Wrong query format")
      return true
    }
    val from: String = command(2)
    val to: String = command(4)
    val fromTime = LocalDateTime.parse(from)
    val toTime = LocalDateTime.parse(to)
    val calls = CallTable().getCallsTimeInterval(fromTime, toTime)
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
    calls.foreach(call => {
      val callerUserRecords = users.usersByPhoneNumber(call.callerNumber)
      var firstName = ""
      var lastName = ""
      if (callerUserRecords.isEmpty) {
        firstName = call.callerNumber
      }else {
        val caller = callerUserRecords(0)
        firstName = caller.firstName
        lastName = caller.lastName
      }
      println(firstName + " | " +
        lastName + " | +" +
        call.calleeNumber + " | " +
        call.time.atOffset(ZoneOffset.ofHours(3)) + " | " +
        call.duration.getSeconds + " | " +
        call.cost)
    })
    true
  }

  object HelpParser extends CommandParser {

    override def help: String = "help \nprint this help message"

    override def parseCommand(command: String): Boolean = {
      println("Commands:")
      parsers.map(parser => parser.help).foreach(println)
      true
    }
  }

  object ExitParser extends CommandParser {

    override def help: String = "exit \nexit interpreter"

    override def parseCommand(command: List[String]): Boolean = false
  }

  object NotFoundParser extends CommandParser {
    override def parseCommand(command: List[String]): Boolean = {
      val commandName = command(0)
      println(s"command \'$commandName\' not found")
      true
    }
  }
}