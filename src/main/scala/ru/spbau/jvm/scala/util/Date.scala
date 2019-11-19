package ru.spbau.jvm.scala.util

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

object Date {
  private val dateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  private val timeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  def fromDate(value: String): LocalDate = {
    LocalDate.parse(value, dateFormatter)
  }

  def toDate(value: String): LocalDate = {
    LocalDate.parse(value, dateFormatter)
  }

  def fromDateTime(value: String): LocalDateTime = {
    LocalDateTime.parse(value, timeFormatter)
  }

  def toDateTime(value: String): LocalDateTime = {
    LocalDateTime.parse(value, timeFormatter)
  }
}
