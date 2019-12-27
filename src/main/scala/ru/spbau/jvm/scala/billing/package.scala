package ru.spbau.jvm.scala

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
    time: String,
    duration: Long,
    cost: BigDecimal
  )
}
