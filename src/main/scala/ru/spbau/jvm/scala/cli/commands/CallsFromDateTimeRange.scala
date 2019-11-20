package ru.spbau.jvm.scala.cli.commands
import org.joda.time.{DateTime, Duration}
import ru.spbau.jvm.scala.db.DB

class CallsFromDateTimeRange extends Command {
  override def exec(db: DB, args: Array[String]): String = {
    try {
      val start: DateTime = DateTime.parse(args(0))
      val finish: DateTime = DateTime.parse(args(1))
      db.data.getActionTable()
        .select("Start", x => {
          // don't catch exception - we check it on the db part
          DateTime.parse(x).isAfter(start) && !DateTime.parse(x).isAfter(finish)
      }).joinBy(db.usedPhone.getActionTable(), "PhoneID")
        .joinBy(db.employee.getActionTable(), "EmployeeID")
        .joinBy(db.operation.getActionTable(), "OperationType")
        .actionWithTwo("Start", "Finish", "Duration(s)",
          (start: String, finish: String) => {
            new Duration(DateTime.parse(start), DateTime.parse(finish)).getStandardSeconds.toString
          })
        .actionWithTwo("CostPerMinute($)", "Duration(s)", "Cost($)",
          (costPerMin: String, duration: String) => {
            var cost = duration.toInt / 60
            if (duration.toInt % 60 != 0) {
              cost += 1
            }
            (cost * costPerMin.toFloat).toString
          })
        .filterCols(List("Cost($)", "Duration(s)", "Callee", "Name", "LastName", "Description"), x => x)
        .getStr()
    } catch {
      case e: IllegalArgumentException =>
        "Invalid CallsFromDateTimeRange args: " + e.getMessage
    }
  }

  def getInfo(): String = {
    "Statistics about calls from time range"
  }
}
