package ru.spbau.jvm.scala

case class Date(private val day: Int,
                private val month: Int,
                private val year: Int) extends Ordering[Date]{

  def <=(other: Date): Boolean = compare(this, other) <= 0

  override def toString: String = s"$day.$month.$year"

  override def compare(x: Date, y: Date): Int =
    if (x.year != y.year) {
      x.year compare y.year
    } else if (x.month != y.month) {
      x.month compare y.month
    } else {
      x.day compare y.day
    }
}

object minDate extends Date(Int.MinValue, Int.MinValue, Int.MinValue)

object maxDate extends Date(Int.MaxValue, Int.MaxValue, Int.MaxValue)

case class DateRange(private val startDate: Date = minDate, private val finishDate: Date = maxDate) {
  def contains(date: Date): Boolean = startDate <= date && date <= finishDate
}