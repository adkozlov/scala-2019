package ru.spbau.jvm.scala.cli.commands
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.jvm.scala.db.DB
import ru.spbau.jvm.scala.db.tables.rows.DataRow.DATE_TIME_PATTERN

class CallsFromDateTimeRange extends Command {
  override def exec(db: DB, args: Array[String]): String = {
    try {
      println(args.size)
      val start: DateTime = DateTime.parse(args(0))
      val finish: DateTime = DateTime.parse(args(1))
      db.data.getActionTable()
        .select("Start", x => {
          // don't catch exception - we check it on the db part
          DateTime.parse(x).isAfter(start) && !DateTime.parse(x).isAfter(finish)
      }).joinBy(db.usedPhone.getActionTable(), "PhoneID")
        .getStr()
    } catch {
      case e: IllegalArgumentException =>
        "Invalid CallsFromDateTimeRange args: " + e.getMessage
    }
  }
}
