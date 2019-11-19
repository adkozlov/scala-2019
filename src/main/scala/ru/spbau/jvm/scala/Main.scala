package ru.spbau.jvm.scala

import java.io.File

import scala.io.StdIn
import scala.util.control.Breaks._

object Main {
    val cmds: Map[String, String] = Map(
        "help" -> "",
        "avg" -> "average call duration with filters",
        "calls" -> "list calls with filters",
        "total" -> "total calls' cost with filters",
        "number" -> "employee's number",
        "max_dur_empl" -> "employee with max total calls' duration with filters",
        "min_cost_empl" -> "employee with min total calls' cost with filters",
        "exit" -> "exit"
    )

    val filters: String = "from DATE\n" +
        "to DATE\n" +
        "by FIRST_NAME LAST_NAME\n" +
        "dur_from FLOAT\n" +
        "dur_to FLOAT"

    def handleHelp(cmd: Array[String]): String = {
        if (cmd.length == 1) {
            "you can use these commands\n" +
            cmds.keys.reduce((a, b) => a + "\n" + b) + "\n" +
            "and these filters\n" +
            filters
        } else {
            cmds(cmd(1))
        }
    }

    def parseFilters(cmd: Array[String]): List[Call => Boolean] = {
        var result: List[Call => Boolean] = List.empty
        var i: Int = 1
        while (i < cmd.length) {
            val add = cmd(i) match {
                case "to" =>
                    val bound = cmd(i + 1)
                    result = result.appended(call => call.date <= bound)
                    2
                case "from" =>
                    val bound = cmd(i + 1)
                    result = result.appended(call => call.date >= bound)
                    2
                case "by" =>
                    val firstName = cmd(i + 1)
                    val secondName = cmd(i + 2)
                    result = result.appended(call => call.firstName == firstName && call.secondName == secondName)
                    3
                case "dur_from" =>
                    val bound = cmd(i + 1)
                    result = result.appended(call => call.duration >= bound.toFloat)
                    2
                case "dur_to" =>
                    val bound = cmd(i + 1)
                    result = result.appended(call => call.duration <= bound.toFloat)
                    2
                case _ => throw new Exception
            }
            i = i + add
        }
        result
    }

    def handleMapReduce(cmd: Array[String], bases: List[CallsBase]): String = {
        try {
            val filters: List[Call => Boolean] = parseFilters(cmd)
            val filtered: LazyList[Call] = bases.iterator
                .flatMap(base => base.calls).filter(call => filters.forall(filter => filter(call))).to(LazyList)
            if (filtered.isEmpty) {
                return "no calls satisfy the filters"
            }
            cmd(0) match {
                case "avg" =>
                    val (sum, cnt) = filtered
                        .map(call => (call.duration, 1.toInt)).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
                    (sum / cnt).toInt.toString
                case "calls" =>
                    filtered.map(call => call.toString).reduce((a, b) => a + "\n" + b)
                case "total" =>
                    filtered.map(call => call.cost).iterator.sum.toString
                case "max_dur_empl" =>
                    val empl = filtered.groupBy(call => (call.firstName, call.secondName)).maxBy(
                        grouped => grouped._2.map(call => call.duration).iterator.sum
                    )._1
                    empl._1 + " " + empl._2
                case "min_cost_empl" =>
                    val empl = filtered.groupBy(call => (call.firstName, call.secondName)).minBy(
                        grouped => grouped._2.map(call => call.cost).iterator.sum
                    )._1
                    empl._1 + " " + empl._2
            }
        } catch {
            case e: Exception => "incorrect format"
        }
    }

    def handleNumber(cmd: Array[String], bases: List[CallsBase]): String = {
        val found = bases.flatMap(base => base.calls)
            .find(call => call.firstName == cmd(1) && call.secondName == cmd(2))
        if (found.isDefined) {
            (found.get.firstName, found.get.secondName).toString()
        } else {
            s"employee '${cmd(1)} ${cmd(2)}' not found"
        }
    }

    def main(args: Array[String]): Unit = {
        val storage: File = new File("./resources")

        val bases: List[CallsBase] = storage.listFiles().to(List).map(f => CallsBase.fromPath(f.toPath))

        while (true) {
            breakable {
                print("> ")
                val cmd: Array[String] = StdIn.readLine().split(" ")

                if (cmd.length == 0 || !cmds.contains(cmd(0))) {
                    println("unknown command")
                    break
                }

                cmd(0) match {
                    case "help" => println(handleHelp(cmd))
                    case "number" => println(handleNumber(cmd, bases))
                    case "exit" => return
                    case _ => println(handleMapReduce(cmd, bases))
                }
            }
        }
    }
}
