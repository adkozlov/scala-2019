package ru.spbau.jvm.scala.homework02

object Main {

class Int10(val value: Int) extends Ordered[Int10] {

  override def compare(that: Int10): Int = {
    (this.value % 10).compare(that.value)
  }
}

  def main(args: Array[String]): Unit = {
    val set = MultiSet[Int](3, 5, 4, 0, 1)
    set.insert(2)
    set.insert(6)
    set.foreach(key => print(key + " "))

    val set1 = MultiSet[Int](3, 5, 4, 0, 1)
    set1.foreach(key => print(key + " "))

    val set3 = set1 & set
    set3.foreach(key => print(key + " "))
  }
}
