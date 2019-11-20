package ru.spbau.jvm.scala

import org.joda.time.DateTime

case class PhoneCall(var phone: String,
                var callee: String,
                var date: DateTime,
                var duration: Int,
                var cost: Float) extends TableEntity

class PhoneCallsTable extends Table[PhoneCall] {

  override def parseEntity(line: String): PhoneCall = {
    line.split(SEPARATOR) match {
      case Array(phone, callee, dateStr, durationStr, costStr) => {

        var date: DateTime = null
        try {
          date = DateTime.parse(dateStr)
        } catch {
          case _: IllegalArgumentException =>
            throw new DatabaseException("Invalid date format. Data should match 'DD.MM.YYYY'.")
        }

        var duration: Int = 0
        try {
          duration = durationStr.toInt
        } catch {
          case _: IllegalArgumentException =>
            throw new DatabaseException("Invalid duration format. Duration should be Int.")
        }

        var cost: Float = 0
        try {
          cost = costStr.toFloat
        } catch {
          case _: IllegalArgumentException =>
            throw new DatabaseException("Invalid cost format. Cost should be float.")
        }

        PhoneCall(phone, callee, date, duration, cost)
      }
      case _ =>
        throw new DatabaseException("Invalid line format. Line should match (phone,callee,date,duration,cost).")
    }
  }
}
