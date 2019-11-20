package ru.spbau.jvm.scala

import java.util.Date

case class Call(caller: Employee, callee: String, date: Date, duration: Int, cost: Double) {
  def toStringWithoutEmployee: String = callee + " | " + date + " | " + duration + " | " + cost

  def toStringWithoutDate: String = caller.firstName + " | " + caller.lastName + " | " +
    callee + " | "  + "| " + duration + " | " + cost
}