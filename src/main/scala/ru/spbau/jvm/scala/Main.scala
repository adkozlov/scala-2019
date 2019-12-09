package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.multiset.MultiSet

object Main {
  def main(args: Array[String]): Unit = {
    val set = MultiSet(4, 8, 15, 16, 23, 42, 42)
    println(set(42))

    val intersect = set | MultiSet(42)
    println(intersect(42))
  }
}
