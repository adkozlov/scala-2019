import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.io.StdIn

object Main {

    private val CALLS_LIST_HEADER = "FirstName | LastName | Callee | Duration (s) | Cost ($) | Date (dd.MM.uuuu)"
    private val DATE_PATTERN = "(\\d{2}).(\\d{2}).(\\d{4})"
    private val CALLS_FROM_TO_RE = f"calls from $DATE_PATTERN to $DATE_PATTERN".r
    private val AVG_RE = "avg".r
    private val MIN_RE = "min".r
    private val MAX_RE = "max".r
    private val TOTAL_FROM_TO_RE = f"total from $DATE_PATTERN to $DATE_PATTERN".r
    private val TOTAL_FROM_RE = f"total from $DATE_PATTERN".r
    private val TOTAL_RE = f"total".r
    private val NUMBER_RE = f"number ([a-zA-Z]+) ([a-zA-Z]+)".r
    private val SORTED_RE = f"sorted from $DATE_PATTERN to $DATE_PATTERN".r
    private val HELP_MESSAGE =
        """HELP
          |calls from FROM_DATE to TO_DATE
          |avg (duration, s)
          |min (duration, s)
          |max (duration, s)
          |total from FROM_DATE to TO_DATE (cost, $)
          |total from DROM_DATE (cost, $)
          |total (cost, $)
          |number NAME SURNAME
          |sorted from FROM_DATE to TO_DATE (by date)
          |help
          |exit
          |
          |DATE example: 20.10.2019 (dd.mm.yyyy)
        """.stripMargin

    private val dataBase = new DataBase

    def main(args: Array[String]): Unit = {
        try {
            dataBase.loadDataBase("resources/calls.txt", "resources/matching.txt")
        } catch {
            case e: Exception =>
                println(e.getMessage)
                return
        }
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

    def printCall(call: Call): Unit = {
        val userFrom = dataBase.getUserById(call.idFrom)
        val userTo = dataBase.getUserById(call.idTo)
        println(
            s"${userFrom.name} | " +
                s"${userFrom.surname} | " +
                s"${userTo.number} | " +
                s"${call.duration} | " +
                s"${call.cost} | " +
                s"${call.date.format(DateTimeFormatter.ofPattern("dd.MM.uuuu"))}"
        )
    }

    def processCallsFromTo(from: LocalDate, to: LocalDate): Unit = {
        println(CALLS_LIST_HEADER)
        dataBase.getCallsInPeriod(from, to).forEach(printCall)
    }

    def processAvg(): Unit = {
        val calls = dataBase.getCallsInPeriod(LocalDate.MIN, LocalDate.MAX)
        println(s"${calls.stream().mapToDouble(x => x.duration).sum() / calls.size()}s")
    }

    def processMin(): Unit = {
        println(
            s"${dataBase.getCallsInPeriod(LocalDate.MIN, LocalDate.MAX)
                .stream()
                .mapToDouble(x => x.duration)
                .min().getAsDouble}s"
        )
    }

    def processMax(): Unit = {
        println(
            s"${dataBase.getCallsInPeriod(LocalDate.MIN, LocalDate.MAX)
                .stream()
                .mapToDouble(x => x.duration)
                .max().getAsDouble}s"
        )
    }

    def processTotalFromTo(from: LocalDate, to: LocalDate): Unit = {
        println(
            s"${dataBase.getCallsInPeriod(from, to)
                .stream()
                .mapToDouble(x => x.cost)
                .sum()}S"
        )
    }

    def processTotalFrom(from: LocalDate): Unit = {
        processTotalFromTo(from, LocalDate.MAX)
    }

    def processTotal(): Unit = {
        processTotalFromTo(LocalDate.MIN, LocalDate.MAX)
    }

    def processNumber(name: String, surname: String): Unit = {
        try {
            val user = dataBase.getUserByNameSurname(name, surname)
            println(s"${user.number}")
        } catch {
            case _: Exception =>
                throw new IllegalArgumentException(s"employee '$name $surname' not found")
        }
    }

    def processSorted(from: LocalDate, to: LocalDate): Unit = {
        dataBase.getCallsInPeriod(from, to)
            .stream()
            .sorted((o1: Call, o2: Call) => o1.date.compareTo(o2.date))
            .forEach(printCall)
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
        case SORTED_RE(fromDay, fromMonth, fromYear, toDay, toMonth, toYear) =>
            processSorted(
                LocalDate.of(fromYear.toInt, fromMonth.toInt, fromDay.toInt),
                LocalDate.of(toYear.toInt, toMonth.toInt, toDay.toInt)
            )
        case _ => throw new IllegalArgumentException(f"command '$command' not found")
    }
}
