import java.time.LocalDate

import scala.io.StdIn

object Main {
    private val DATE_PATTERN = "(\\d{2}).(\\d{2}).(\\d{4})"
    private val CALLS_FROM_TO_RE = f"calls from $DATE_PATTERN to $DATE_PATTERN".r
    private val AVG_RE = "avg".r
    private val MIN_RE = "min".r
    private val MAX_RE = "max".r
    private val TOTAL_FROM_TO_RE = f"total from $DATE_PATTERN to $DATE_PATTERN".r
    private val TOTAL_FROM_RE = f"total from $DATE_PATTERN".r
    private val TOTAL_RE = f"total".r
    private val NUMBER_RE = f"number ([a-zA-Z]+) ([a-zA-Z]+)".r
    private val SORTED_RE = f"sorted from $DATE_PATTERN to $DATE_PATTERN by ([a-zA-Z]+)".r
    private val HELP_MESSAGE =
        """HELP
          |a
          |b
          |c
        """.stripMargin

    def main(args: Array[String]): Unit = {
        while (true) {
            val command = StdIn.readLine()
            command match {
                case "exit" => return
                case "help" => println(HELP_MESSAGE)
                case _ =>
                    try {
                        process(command)
                    } catch {
                        case e: Exception => println(e.getMessage)
                    }
            }
        }
    }

    def processCallsFromTo(from: LocalDate, to: LocalDate): Unit = {
        println(f"calls from $from to $to")
    }

    def processAvg(): Unit = {
        println("avg")
    }

    def processMin(): Unit = {
        println("min")
    }

    def processMax(): Unit = {
        println("max")
    }

    def processTotalFromTo(from: LocalDate, to: LocalDate): Unit = {
        println(f"total from $from to $to")
    }

    def processTotalFrom(from: LocalDate): Unit = {
        println(f"total from $from")
    }

    def processTotal(): Unit = {
        println("total")
    }

    def processNumber(name: String, surname: String): Unit = {
        println(f"number of $name $surname")
    }

    def processSorted(from: LocalDate, to: LocalDate, sortingType: String): Unit = {
        println(f"sorted from $from to $to by $sortingType")
    }

    def process(command: String): Unit = command match {
        case CALLS_FROM_TO_RE(fromDay, fromMonth, fromYear, toDay, toMonth, toYear) =>
            processCallsFromTo(
                LocalDate.of(fromYear.toInt, fromMonth.toInt, fromDay.toInt),
                LocalDate.of(toYear.toInt, toMonth.toInt, toDay.toInt)
            )
        case AVG_RE() =>
            processAvg()
        case MIN_RE() =>
            processMin()
        case MAX_RE() =>
            processMax()
        case TOTAL_FROM_TO_RE(fromDay, fromMonth, fromYear, toDay, toMonth, toYear) =>
            processTotalFromTo(
                LocalDate.of(fromYear.toInt, fromMonth.toInt, fromDay.toInt),
                LocalDate.of(toYear.toInt, toMonth.toInt, toDay.toInt)
            )
        case TOTAL_FROM_RE(fromDay, fromMonth, fromYear) =>
            processTotalFrom(LocalDate.of(fromYear.toInt, fromMonth.toInt, fromDay.toInt))
        case TOTAL_RE() =>
            processTotal()
        case NUMBER_RE(name, surname) =>
            processNumber(name, surname)
        case SORTED_RE(fromDay, fromMonth, fromYear, toDay, toMonth, toYear, sortingType) =>
            processSorted(
                LocalDate.of(fromYear.toInt, fromMonth.toInt, fromDay.toInt),
                LocalDate.of(toYear.toInt, toMonth.toInt, toDay.toInt),
                sortingType
            )
        case _ => throw new IllegalArgumentException(f"command '$command' not found")
    }
}
