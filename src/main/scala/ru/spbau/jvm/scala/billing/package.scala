package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date

package object billing {
  case class PhoneNumber(
    number: String
  )

  case class Employee(
    firstName: String,
    lastName: String
  )

  case class Call(
    caller: Employee,
    callee: PhoneNumber,
    time: Date,
    duration: Long,
    cost: BigDecimal
  )

  case class PhoneNumberRecord(
    id: Long,
    number: String
  )

  case class EmployeeRecord(
    firstName: String,
    lastName: String,
    phoneNumberId: Long
  )

  case class CallRecord(
    callerPhoneNumber: String,
    calleePhoneNumber: String,
    time: Date,
    duration: Long,
    cost: BigDecimal
  )

  implicit class StringExt(private val string: String) extends AnyVal {
    def toDate: Date = {
      new SimpleDateFormat("dd.MM.yyyy").parse(string)
    }

    def toDateTime: Date = {
      new SimpleDateFormat("dd.MM.yyyy HH:mm:ss").parse(string)
    }
  }

  private implicit class DateExt(private val date: Date) extends AnyVal {
    def in(from: Option[Date], to: Option[Date]): Boolean = {
      (from.isEmpty || date.after(from.get)) && (to.isEmpty || date.before(to.get))
    }
  }
}
