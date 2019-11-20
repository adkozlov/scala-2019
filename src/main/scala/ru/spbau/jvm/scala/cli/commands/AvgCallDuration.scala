package ru.spbau.jvm.scala.cli.commands

import org.joda.time.{DateTime, Duration}
import ru.spbau.jvm.scala.db.DB

class AvgCallDuration extends Command {
  override def exec(db: DB, args: Array[String]): String = {
    try {
      val resBody: List[Int] = db.data.getActionTable()
        .actionWithTwo("Start", "Finish", "Duration(s)",
          (start: String, finish: String) => {
            new Duration(DateTime.parse(start), DateTime.parse(finish)).getStandardSeconds.toString
          })
        .filterCols(List("Duration(s)"), x => x)
        .getBody()
        .map(x => x(0).toInt)
        .toList
      "Avg(s)\n" + (resBody.sum / resBody.size).toString
    } catch {
      case e: IllegalArgumentException =>
        "Invalid AvgCallDuration args: " + e.getMessage
    }
  }

  override def getInfo(): String = {
    "Average value of calls duration"
  }
}
