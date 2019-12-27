package ru.spbau.jvm.scala.billing

import java.io.File
import java.text.SimpleDateFormat
import java.util.NoSuchElementException

import scala.io.StdIn

object Main {
  private final val HELP_MESSAGE = "Supported requests:\n" +
    "- List of all calls in specified time period:\n" +
    "calls [from DD.MM.YYYY] [to DD.MM.YYYY]\n" +
    "- Incoming calls of specified employee:\n" +
    "incoming calls <Name> <Surname> from DD.MM.YYYY to DD.MM.YYYY\n" +
    "- Outgoing calls of specified employee:\n" +
    "outgoing calls <Name> <Surname> from DD.MM.YYYY to DD.MM.YYYY\n" +
    "- Average cost of all calls:\n" +
    "avg\n" +
    "- Total cost of all calls in specified time period:\n" +
    "total [from DD.MM.YYYY] [to DD.MM.YYYY]\n" +
    "- Specified employee number:\n" +
    "number <Name> <Surname>\n" +
    "- Owner of specified number:\n" +
    "owner <number>\n" +
    "- Exit:\n" +
    "exit\n" +
    "- This message:\n" +
    "help\n"

  private val DATE_FORMAT = "dd.MM.yyyy"

  private val NAME_PATTERN = "([A-Za-z'-]+)"
  private val NUMBER_PATTERN = "([0-9+()-]+)"
  private val DATE_PATTERN = "([0-9.]+)"
  private val callsFromToPattern = s"^calls from $DATE_PATTERN to $DATE_PATTERN$$".r
  private val callsFromPattern = s"^calls from $DATE_PATTERN$$".r
  private val callsToPattern = s"^calls to $DATE_PATTERN$$".r
  private val callsPattern = s"^calls".r
  private val incomingCallsPattern = s"^incoming calls $NAME_PATTERN $NAME_PATTERN$$".r
  private val outgoingCallsPattern = s"^outgoing calls $NAME_PATTERN $NAME_PATTERN$$".r
  private val avgPattern = s"^avg$$".r
  private val totalFromToPattern = s"^total from $DATE_PATTERN to $DATE_PATTERN$$".r
  private val totalFromPattern = s"^total from $DATE_PATTERN$$".r
  private val totalToPattern = s"^total to $DATE_PATTERN$$".r
  private val totalPattern = s"^total$$".r
  private val numberPattern = s"^number $NAME_PATTERN $NAME_PATTERN$$".r
  private val ownerPattern = s"^owner $NUMBER_PATTERN$$".r
  private val exitPattern = s"^exit$$".r
  private val helpPattern = s"^help$$".r

  def main(args: Array[String]): Unit = {
    val stats = BillingStats(new File("resources"))
    val dateFormat = new SimpleDateFormat(DATE_FORMAT)
    var toExit = false

    print(HELP_MESSAGE)
    while (!toExit) {
      val line = StdIn.readLine()
      line match {
        case callsFromToPattern(from, to) =>
          printCalls(stats.getCalls(Some(dateFormat.parse(from)), Some(dateFormat.parse(to))))
        case callsFromPattern(from) =>
          printCalls(stats.getCalls(Some(dateFormat.parse(from)), None))
        case callsToPattern(to) =>
          printCalls(stats.getCalls(None, Some(dateFormat.parse(to))))
        case callsPattern() =>
          printCalls(stats.getCalls(None, None))
        case incomingCallsPattern(firstName, lastName) =>
          try {
            printCalls(stats.getIncomingCalls(Employee(firstName, lastName)))
          } catch {
            case _: NoSuchElementException =>
              println(s"employee '$firstName $lastName' not found")
          }
        case outgoingCallsPattern(firstName, lastName) =>
          try {
            printCalls(stats.getOutgoingCalls(Employee(firstName, lastName)))
          } catch {
            case _: NoSuchElementException =>
              println(s"employee '$firstName $lastName' not found")
          }
        case avgPattern() =>
          println(s"${stats.getAverageDuration}s")
        case totalFromToPattern(from, to) =>
          println(s"$$${stats.getTotalCost(Some(dateFormat.parse(from)), Some(dateFormat.parse(to)))}")
        case totalFromPattern(from) =>
          println(s"$$${stats.getTotalCost(Some(dateFormat.parse(from)), None)}")
        case totalToPattern(to) =>
          println(s"$$${stats.getTotalCost(None, Some(dateFormat.parse(to)))}")
        case totalPattern() =>
          println(s"$$${stats.getTotalCost(None, None)}")
        case numberPattern(firstName, lastName) =>
          try {
            println(stats.getNumber(Employee(firstName, lastName)).number)
          } catch {
            case _: NoSuchElementException =>
              println(s"employee '$firstName $lastName' not found")
          }
        case ownerPattern(number) =>
          try {
            stats.getOwner(PhoneNumber(number)) match {
              case Some(owner) =>
                println(s"${owner.firstName} ${owner.lastName}")
              case None =>
                println("number does not belong to any employee")
            }
          } catch {
            case _: NoSuchElementException =>
              println(s"not a reserved number")
          }
        case exitPattern() =>
          toExit = true
        case helpPattern() =>
          println(HELP_MESSAGE)
        case command @ _ =>
          println(s"command '$command' not found")
      }
    }
  }

  private def printCalls(calls: List[Call]): Unit = {
    if (calls.isEmpty) {
      println("No calls")
    } else {
      println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
      calls.foreach(c =>
        println(s"${c.caller.firstName} | ${c.caller.lastName} | ${c.callee.number} | ${c.duration} | ${c.cost}")
      )
    }
  }
}
