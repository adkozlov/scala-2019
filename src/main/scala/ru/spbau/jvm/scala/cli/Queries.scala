package ru.spbau.jvm.scala.cli

import java.util.Date

import ru.spbau.jvm.scala.billing._

import scala.util.{Failure, Success, Try}

/**
 * @param leftBound  if `None` the range is from the beginning of time
 * @param rightBound if `None` the range is to the end of time
 */
case class DateRange(leftBound: Option[Date] = None,
                     rightBound: Option[Date] = None) {
  override def toString: String = {
    val showLeft = if (leftBound.isEmpty) "..." else leftBound.get.toString
    val showRight = if (rightBound.isEmpty) "..." else rightBound.get.toString
    s"$showLeft -- $showRight"
  }
}

case class PersonName(firstName: String, lastName: String) {
  override def toString: String = s"$firstName $lastName"
}

trait Query {
  def execute(context: BillingSystem)
}

class CallQuery(private val dateRange: DateRange) extends Query {
  override def execute(context: BillingSystem): Unit = {
    val result = context.calls(dateRange)
    println("FirstName LastName Phone Duration Cost")
    println(result.map(_.mkString(" ")).mkString("\n"))
  }
}

class MessageQuery(private val person: PersonName,
                   private val dateRange: DateRange) extends Query {
  override def execute(context: BillingSystem): Unit = {
    println("Messages!")
    println(context.messages(person, dateRange))
  }
}

abstract class AvgQuery(private val dateRange: DateRange) extends Query {
  def applyContext(system: BillingSystem, range: DateRange): Double

  override def execute(context: BillingSystem): Unit = {
    Try(applyContext(context, dateRange)) match {
      case Success(avg) => println(avg)
      case Failure(ex) => println(s"Error: ${ex.getMessage}")
    }
  }
}

class AvgDurationQuery(private val dateRange: DateRange) extends AvgQuery(dateRange) {
  override def applyContext(context: BillingSystem, range: DateRange): Double = context.avgDuration(dateRange)
}

class AvgCostQuery(private val dateRange: DateRange) extends AvgQuery(dateRange) {
  override def applyContext(context: BillingSystem, range: DateRange): Double = context.avgCost(dateRange)
}


class TotalQuery(dateRange: DateRange) extends Query {
  override def execute(context: BillingSystem): Unit = {
    println(context.total(dateRange))
  }
}

class NumberQuery(private val person: PersonName) extends Query {
  override def execute(context: BillingSystem): Unit = {
    println(context.number(person).mkString("\n"))
  }
}

class ContactQuery(private val caller: PersonName,
                   private val callee: PersonName,
                   private val dateRange: DateRange) extends Query {
  override def execute(context: BillingSystem): Unit = {
    println(context.contact(caller, callee, dateRange))
  }
}

class HelpQuery extends Query {
  override def execute(context: BillingSystem): Unit = {
    val help =
      """BillingSystem(R) Repl Help Message
        | Commands:
        | - avg [duration|cost] [from <date> [to <date>]]  average call duration/cost between two dates
        | - calls [from <date> [to <date>]]                all calls between two dates (optionally)
        | - number <name>                                  find phone number/numbers of a person with the given name
        | - total [from <date> [to <date>]]                total cost of all operations between two dates (optionally)
        | - message <name> [from <date [to <date>]]        how many messages the person sent and got
        | - contact <name> to <name> [from <date> [to <date>]]  number of time when the first person made contact with
        |                                                       the second one
        | - help                                           prints this help
        | Date Format:
        |  Only dates of format "yyyy-MM-dd" are accepted.
        | Person Name Format:
        |  Person's name consists of two parts: her first name and her last name.
        |""".stripMargin
    print(help)
  }
}
