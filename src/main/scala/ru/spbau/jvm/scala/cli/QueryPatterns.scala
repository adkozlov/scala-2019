package ru.spbau.jvm.scala.cli

import scala.util.matching.Regex

object QueryPatterns {
  val DatePattern = """(\d\d\.\d\d.\d\d\d\d)"""
  val PhonePattern = """(\+[\d ][\d- ]*)"""
  val NamePattern = """(\w*)"""

  val ListCallsBetweenDates: Regex = s"""calls from $DatePattern to $DatePattern""".r
  val AverageCallDuration: Regex = """avg""".r
  val TotalCostBetweenDates: Regex = s"""total from $DatePattern to $DatePattern""".r
  val EmployeePhone: Regex = s"""number $NamePattern $NamePattern""".r
  val UserWithMaxSpending: Regex = s"""user-max-spending""".r
  val UserWithMaxDuration: Regex = s"""user-max-total-duration""".r
  val WhichPhone: Regex = s"""which phone $PhonePattern""".r
  val Help: Regex = """help""".r
  val Exit: Regex = """exit""".r
}
