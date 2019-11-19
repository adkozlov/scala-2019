package ru.spbau.jvm.scala

import java.time.LocalDateTime

import ru.spbau.jvm.scala.CallsDB.{Call, CallDB, EmployeeDB, Event, Name, PhoneNumberDB}

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import scala.reflect.ClassTag

/**
 * This class provides methods which collect statistics of calls made by Company's employees.
 */
final class CallsDB(employeesArray: Array[EmployeeDB], numbersArray: Array[PhoneNumberDB], callsArray: Array[CallDB]) {
  private val nameToNumber: Map[Name, String] = {
    val numberIdToNumber = numbersArray.map { phoneNumber =>
      (phoneNumber.id, phoneNumber.number)
    }.toMap

    employeesArray.map { employee =>
      (Name(employee.firstName, employee.lastName), numberIdToNumber(employee.numberId))
    }.toMap
  }

  private val numberToName: Map[String, Option[Name]] = {
    val numberIdToName = employeesArray.map { employee =>
      (employee.numberId, Name(employee.firstName, employee.lastName))
    }.toMap

    numbersArray.map { phoneNumber =>
      (phoneNumber.number, numberIdToName get phoneNumber.id)
    }.toMap
  }

  private val calls: Array[CallDB] = callsArray.sorted

  /**
   * Finds all calls made during a specified period. The computational complexity of the method is O(log(n) + k) where n
   * is the number of all calls in the db and k is the number of calls made during the specified period.
   */
  def getCalls(from: LocalDateTime = LocalDateTime.MIN, to: LocalDateTime = LocalDateTime.MAX): Seq[Call] = {
    getCallsInPeriod(from, to).map(convertCallDbToCall)
  }

  /**
   * Computes total cost of calls made during the specified period.
   * The computational complexity of the method is O(log(n) + k) where n is the number of all calls in the db
   * and k is the number of calls made during the specified period.
   */
  def getTotal(from: LocalDateTime = LocalDateTime.MIN, to: LocalDateTime = LocalDateTime.MAX): Double = {
    getCallsInPeriod(from, to).foldLeft(0: Double) {(accumulator, callDb) =>
      accumulator + callDb.cost
    }
  }

  /**
   * Computes the average duration of calls made during the specified period.
   * The computational complexity of the method is O(log(n) + k) where n is the number of all calls in the db
   * and k is the number of calls made during the specified period.
   */
  def getAvg(from: LocalDateTime = LocalDateTime.MIN, to: LocalDateTime = LocalDateTime.MAX): Double = {
    // iterative average
    getCallsInPeriod(from, to).foldLeft((0: Double, 1: Int)) {(accumulator, callDb) =>
      val duration = callDb.duration
      val currentAvg = accumulator._1
      val step = accumulator._2
      (currentAvg + (duration - currentAvg) / step, step + 1)
    }._1
  }

  /**
   * Finds the phone number of the specified employee.
   * Throws NoSuchElementException if that employee is not found in db.
   * The computational complexity is O(1).
   */
  @throws[NoSuchElementException]
  def getNumber(name: Name): String = nameToNumber(name)

  /**
   * Finds the employee with the specified phone number.
   * Returns None if no one uses this phone number.
   * Throws NoSuchElementException if that phone number is unknown.
   * The computational complexity is O(1).
   */
  @throws[NoSuchElementException]
  def getEmployee(number: String): Option[Name] = numberToName(number)

  /**
   * Finds all calls made by the specified employee.
   * The computational complexity of the method is O(log(n) + k) where n is the number of all calls in the db
   * and k is the number of calls made during the specified period.
   */
  def getOutgoing(name: Name,
                  from: LocalDateTime = LocalDateTime.MIN,
                  to: LocalDateTime = LocalDateTime.MAX): Seq[Call] = {
    getCallsInPeriod(from, to).filter { callDb =>
      numberToName(callDb.fromNumber) contains name
    }.map(convertCallDbToCall)
  }

  /**
   * Finds all calls received by the specified employee.
   * Throws NoSuchElementException if that employee is not found in db.
   * The computational complexity of the method is O(log(n) + k) where n is the number of all calls in the db
   * and k is the number of calls made during the specified period.
   */
  def getIncoming(name: Name,
                  from: LocalDateTime = LocalDateTime.MIN,
                  to: LocalDateTime = LocalDateTime.MAX): Seq[Call] = {
    getCallsInPeriod(from, to).filter { callDb =>
      numberToName.get(callDb.toNumber).flatten contains name
    }.map(convertCallDbToCall)
  }

  private def getCallsInPeriod(from: LocalDateTime, to: LocalDateTime): Array[CallDB] = {
    val startEvent = new Event {
      override val getTime: LocalDateTime =
        if (from == LocalDateTime.MIN) LocalDateTime.MIN else from.minusNanos(1)
    }
    val endEvent = new Event {
      override val getTime: LocalDateTime =
        if (to == LocalDateTime.MAX) LocalDateTime.MAX else to.plusNanos(1)
    }

    // uses binary search
    val startIndex = calls.search(startEvent).insertionPoint
    val endIndex = calls.search(endEvent).insertionPoint
    calls.slice(startIndex, endIndex)
  }

  private def convertCallDbToCall(callDb: CallDB): Call = {
    Call(
      numberToName(callDb.fromNumber).getOrElse(Name("Unknown", "Unknown")),
      callDb.toNumber,
      callDb.duration,
      callDb.cost
    )
  }
}

object CallsDB {
  case class Name(firstName: String, lastName: String)
  case class Call(caller: Name, callee: String, duration: Int, cost: Float)

  // those case classes represent raws in csv-db files
  private case class EmployeeDB(firstName: String, lastName: String, numberId: Int)
  private case class PhoneNumberDB(id: Int, number: String)

  private trait Event {
    val getTime: LocalDateTime
  }

  implicit def orderingByTime[A <: Event]: Ordering[A] = Ordering.fromLessThan(_.getTime isBefore _.getTime)

  private case class CallDB(fromNumber: String,
                            toNumber: String,
                            timeStart: LocalDateTime,
                            duration: Int,
                            cost: Float) extends Event {
    override val getTime: LocalDateTime = timeStart
  }

  def apply(): CallsDB = {
    val employeesTry = readFromCSV("employees.txt") { columns =>
      val Array(firstName, lastName, numberId) = columns
      EmployeeDB(firstName, lastName, numberId.toInt)
    }

    val numbersTry = readFromCSV("numbers.txt") { columns =>
      val Array(id, number) = columns
      PhoneNumberDB(id.toInt, number)
    }

    val callsTry = readFromCSV("calls.txt") { columns =>
      val Array(fromNumber, toNumber, timeStart, duration, cost) = columns
      CallDB(fromNumber, toNumber, LocalDateTime.parse(timeStart), duration.toInt, cost.toFloat)
    }

    (employeesTry, numbersTry, callsTry) match {
      case (Success(a), Success(b), Success(c)) => new CallsDB(a, b, c)
      case (Failure(e), _, _) => throw getDbException(e)
      case (_, Failure(e), _) => throw getDbException(e)
      case (_, _, Failure(e)) => throw getDbException(e)
    }
  }

  private def readFromCSV[B](fileName: String)(mapFun: Array[String] => B)(implicit c: ClassTag[B]): Try[Array[B]] = {
    Using(Source.fromFile(s"resources/$fileName")) { file =>
      file.getLines().map { line =>
        val columns = line.split(";").map(_.trim())
        mapFun(columns)
      }.toArray
    }
  }

  private def getDbException(throwable: Throwable): RuntimeException = {
    new RuntimeException("Error reading from db", throwable)
  }
}
