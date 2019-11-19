package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.command.{Avg, BiggestTalker, CallsTotalFrom, CostTotalFrom, Help, Max, MostCalls, Number}

import scala.io.StdIn

object Main {
  private val From = "from"
  private val To = "to"

  private def tailFromTo(tail: List[String]) = {
    tail match {
      case From :: from :: To :: to :: _ => List(from, to)
      case From :: from :: _ => List(from)
      case _ => List()
    }
  }

  private def eitherToStdout(e: Either[String, String]): Unit = e match {
    case Left(e) => println(e)
    case Right(res) => println(res)
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      eitherToStdout(StdIn.readLine().split("\\s+").toList match {
        case CallsTotalFrom.name :: tail => CallsTotalFrom.execute(tailFromTo(tail))
        case CostTotalFrom.name :: tail => CostTotalFrom.execute(tailFromTo(tail))
        case Avg.name :: _ => Avg.execute(Nil)
        case Max.name :: _ => Max.execute(Nil)
        case BiggestTalker.name :: _ => BiggestTalker.execute(Nil)
        case MostCalls.name :: _ => MostCalls.execute(Nil)
        case Number.name :: name :: surname :: _ => Number.execute(List(name, surname))
        case Help.name :: _ => Help.execute(Nil)
        case x :: _ => Left("Unknown command " + x)
        case _ => Left("Enter the command")
      })
    }
  }
}
