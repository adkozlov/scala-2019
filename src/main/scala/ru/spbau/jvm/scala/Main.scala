package ru.spbau.jvm.scala
import java.time.format.DateTimeFormatter
import java.time._
import scala.io.StdIn._

object Main {
    private def help(): Unit = println("""
        Available commands:

        calls [from <start date>] [to <end date>] -- print table of all calls in the period

        avg [from <start date>] [to <end date>] -- average of calls duration in the period

        total [from <start date>] [to <end date>] -- total cost of calls in the period

        count [from <start date>] [to <end date>] -- total count of calls in the period
        
        number <first name> <second name> -- get a number of employees

        employee <number> -- get owner of the number

        list <fist name> <second name> -- get list of all employee calls

        help -- print this message

        Date format -- dd.MM.yyyy
        Phone number format -- +7.......... or 8..........
    

        Examples:

        $ calls from 01.09.2019 to 02.09.2019
        FirstName | LastName | Callee | Duration (s) | Cost ($)
        
        John | Doe | +7 900 765 43 21 | 42 | 0.42
        John | Doe | +7 900 123 43 21 | 24 | 0.24
        
        
        $ avg
        242s

        
        $ total from 01.09.2019
        4242


        $ number John Doe
        +7 900 123 45 67 

        $ count 
        10
        """)

    private def printAllCalls(table: Table, args: Array[String]): Unit = {
        val (start, end) = parseTimePeriod(args)
        println("FirstName | LastName | Callee | Duration (s) | Cost ($)")    
        for ( (employee, call) <- table.calls(start, end)) {
            println(employee.name + " | " + employee.secondName + " | " + call.number + " | " + call.duration + " | " + call.cost)
        }
    }

    private def parseTimePeriod(args: Array[String]): (LocalDate, LocalDate) = {
        val format = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        if (args.length == 4 && args(0) == "from" && args(2) == "to") {
            (LocalDate.parse(args(1), format), LocalDate.parse(args(3), format))
        } else if (args.length == 2 && args(0) == "from") {
            (LocalDate.parse(args(1), format), null)
        } else if (args.length == 2 && args(0) == "to") {
            (null, LocalDate.parse(args(1), format))
        } else if (args.length == 0) {
            (null, null)
        } else {
            throw new IllegalArgumentException("Invalid command parameters")
        }
    }

    def main(args: Array[String]): Unit = {
        val table = new Table(InputReader.readEmployees("resources/employees.txt"), InputReader.readCalls("resources/calls.txt"))
        while (true) {
            val input = readLine().split(" ")
            if (input.isEmpty) {
                println("Empty command is not supported")
            }
            val command = input(0)
            val commandArgs = input.drop(1)
            command match {
                case "help" => help()
                case "avg" =>
                    val (start, end) = parseTimePeriod(commandArgs)
                    println(table.avg(start, end) + "s")
                case "number" =>
                    if (commandArgs.length == 2) {
                        val number = table.number(commandArgs(0), commandArgs(1))
                        if (number != null) {
                            println(number)
                        } else {
                            println("employee '" + commandArgs(0) + " " + commandArgs(1) + "' not found")
                        }
                    } else {
                        help()
                    }
                case "total" =>
                    val (start, end) = parseTimePeriod(commandArgs)
                    println(table.total(start, end))
                case "count" =>
                    val (start, end) = parseTimePeriod(commandArgs)
                    println(table.count(start, end))
                case "calls" => printAllCalls(table, commandArgs)
                case "employee" =>
                    val employee = table.numberHolder(commandArgs(0))
                    print(if (employee == null) "No employee for such phone number" else employee.name + " " + employee.secondName)
                case "list" =>
                    if (commandArgs.length != 2) {
                        println("Invalid list arguments")
                    } else {
                        val list = table.callsByEmployee(commandArgs(0), commandArgs(1))
                        if (list == null) println("Employee not found") else {
                            println("Phone number| Date | Duration | Cost")
                            list.foreach(call => println(call.number + " | " + call.date + " | " + call.duration + " | " + call.cost))
                        }
                    }
                case _ => println("Command " + command + " not found")
            }
        }
    }
}
