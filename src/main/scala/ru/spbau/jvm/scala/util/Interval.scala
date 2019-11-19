package ru.spbau.jvm.scala.util

import java.time.LocalDate

import ru.spbau.jvm.scala.storage.db.orm.Call

object Interval {
  def parse(array: Array[String]): (Option[LocalDate], Option[LocalDate]) = {
    val from = if (array.length >= 2 && array(0) == "from") {
      Option.apply(Date.fromDate(array(1)))
    } else {
      Option.empty
    }
    val to = if (array.length >= 2 && array(0) == "to") {
      Option.apply(Date.fromDate(array(1)))
    } else if (array.length >= 4 && array(2) == "to") {
      Option.apply(Date.fromDate(array(3)))
    } else {
      Option.empty
    }

    (from, to)
  }

  def filterCalls(from: Option[LocalDate], to: Option[LocalDate], calls: Iterable[Call]): Iterable[Call] = {
    calls
      .filter((it: Call) =>
        it.timestamp.isAfter(from.getOrElse(LocalDate.MIN).atStartOfDay())
          && it.timestamp.isBefore(to.getOrElse(LocalDate.MAX.minusDays(1)).plusDays(1).atStartOfDay()))

  }
}
