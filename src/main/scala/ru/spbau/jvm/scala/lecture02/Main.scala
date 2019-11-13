package ru.spbau.jvm.scala.lecture02

import java.lang.Iterable

import scala.annotation.tailrec

object Main {

  import IntList._

  def main(args: Array[String]): Unit = {
    printArgs(if (args.isEmpty /* args.length == 0 */ ) defaultArgs else args)

    try {
      factorial(100000)
    } catch {
      case _: StackOverflowError =>
        println("stack overflow")
        println(tailrecFactorial(100000))
    } finally {
      println()
    }

    val list = Cons(1, Cons(2, Cons(3, Nil)))
    foreach(list)(println)
  }

  private def printArgs(args: Array[_]): Unit = {
    def printArgsLoop(): Unit = {
      var index = 0
      while (index < args.length) {
        println(args(index))
        index += 1 // operator ++ does not exist
      }
      println()
    }

    @tailrec
    def printArgs(index: Int = 0): Unit =
      if (index < args.length) {
        val arg = args(index) // args.apply(index)
        println(arg)
        printArgs(index + 1)
      } else {
        println()
      }

    printArgs()
  }

  private def defaultArgs: Array[Int] = /* Array(4, 8, 15, 16, 23, 42) */ {
    val result = new Array[Int](6)
    result(0) = 4 // result.update(0, 4)
    result(1) = 8
    result(2) = 15
    result(3) = 16
    result(4) = 23
    result(5) = 42
    result
  }

  private def factorial(n: Int): BigInt =
    if (n <= 1) 1
    else factorial(n - 1) * n

  private def iterativeFactorial(n: Int): BigInt = {
    var result = 1
    var i = 0

    while (i < n) {
      result *= i + 1
      i += 1
    }

    result
  }

  @tailrec
  private def tailrecFactorial(n: Int, accumulator: BigInt = 1): BigInt = n match {
    case 0 => accumulator
    case _ => tailrecFactorial(n - 1, accumulator * n)
  }

  // procedure syntax to be deprecated in Scala 3
  private def foreach[T](iterable: Iterable[T])
                        (action: T => Unit) {
    val iterator = iterable.iterator
    while (iterator.hasNext) {
      action(iterator.next())
    }
  }
}
