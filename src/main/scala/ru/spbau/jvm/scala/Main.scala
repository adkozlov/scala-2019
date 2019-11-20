package ru.spbau.jvm.scala

object Main {

  def main(args: Array[String]): Unit = {
    new DatabaseExecutor(new Database()).run()
  }
}
