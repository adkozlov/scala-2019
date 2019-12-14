package ru.spbau.jvm.scala.homework02

object Main {

class Int10(val value: Int) extends Ordered[Int10] {

  override def compare(that: Int10): Int = {
    (this.value % 10).compare(that.value)
  }
}

  def main(args: Array[String]): Unit = {

  }
}
