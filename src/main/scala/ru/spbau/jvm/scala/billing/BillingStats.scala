package ru.spbau.jvm.scala.billing

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Using}

final class BillingStats(
  private val employeeToNumber: Map[Employee, PhoneNumber],
  private val numberToEmployee: Map[PhoneNumber, Option[Employee]],
  private val calls: List[Call]
) {

  import BillingStats._

  def getCalls(from: Option[Date], to: Option[Date]): List[Call] = {
    calls.filter( _.time.in(from, to))
  }

  @throws[NoSuchElementException]
  def getIncomingCalls(employee: Employee): List[Call] = {
    val number = employeeToNumber(employee)
    calls.filter(_.callee == number)
  }

  @throws[NoSuchElementException]
  def getOutgoingCalls(employee: Employee): List[Call] = {
    if (!employeeToNumber.contains(employee)) {
      throw new NoSuchElementException
    }
    calls.filter(_.caller == employee)
  }

  def getAverageDuration: Long = {
    calls.map(_.duration).sum / Math.max(calls.length, 1)
  }

  def getTotalCost(from: Option[Date], to: Option[Date]): BigDecimal = {
    calls.filter(_.time.in(from, to)).map( _.cost).sum
  }

  @throws[NoSuchElementException]
  def getNumber(employee: Employee): PhoneNumber = employeeToNumber(employee)

  @throws[NoSuchElementException]
  def getOwner(phoneNumber: PhoneNumber): Option[Employee] = numberToEmployee(phoneNumber)
}

object BillingStats {

  private val DATE_TIME_FORMAT = "dd.MM.yyyy HH:mm:ss"
  private val PHONE_NUMBERS_FILE = "numbers.txt"
  private val EMPLOYEES_FILE = "employees.txt"
  private val CALLS_FILE = "calls.txt"

  def apply(folder: File): BillingStats = {

    val phoneNumberRecords = readRecords(new File(folder, PHONE_NUMBERS_FILE)) {
      case Array(id, number) =>
        PhoneNumberRecord(id.toLong, number)
    }
    val employeeRecords = readRecords(new File(folder, EMPLOYEES_FILE)) {
      case Array(firstName, lastName, phoneNumberId) =>
        EmployeeRecord(firstName, lastName, phoneNumberId.toLong)
    }
    val callRecords = readRecords(new File(folder, CALLS_FILE)) {
      case Array(callerNumber, calleeNumber, time, duration, cost) =>
        CallRecord(callerNumber, calleeNumber, time.toDateTime, duration.toLong, BigDecimal(cost))
    }

    val idToNumber = phoneNumberRecords.map(r => (r.id, PhoneNumber(r.number))).toMap
    val dateTimeFormat = new SimpleDateFormat(DATE_TIME_FORMAT)

    val employeeToNumber: Map[Employee, PhoneNumber] = {
      employeeRecords.map(r => (Employee(r.firstName, r.lastName), idToNumber(r.phoneNumberId))).toMap
    }

    val numberToEmployee: Map[PhoneNumber, Option[Employee]] = {
      val ownedNumbers = employeeToNumber.to(LazyList).map(_.swap).toMap
      idToNumber.values.map(n => (n, ownedNumbers.get(n))).toMap
    }

    val calls: List[Call] = {
      callRecords.map(
        c => Call(
          numberToEmployee(PhoneNumber(c.callerPhoneNumber)).get,
          PhoneNumber(c.calleePhoneNumber),
          c.time,
          c.duration,
          c.cost
        )
      ).toList
    }

    new BillingStats(employeeToNumber, numberToEmployee, calls)
  }

  private def readRecords[Record:ClassTag](table: File)(unpack: Array[String] => Record): Array[Record] = {
    val result = Using(Source.fromFile(table))(table =>
      table.getLines().map(line => {
        unpack(line.split(",").map(_.trim))
      }).toArray
    )
    result match {
      case Success(records) =>
        records
      case Failure(exception) =>
        throw new RuntimeException("Incorrect table", exception)
    }
  }

  private implicit class DateExt(private val date: Date) extends AnyVal {
    def in(from: Option[Date], to: Option[Date]): Boolean = {
      (from.isEmpty || date.after(from.get)) && (to.isEmpty || date.before(to.get))
    }
  }
}
