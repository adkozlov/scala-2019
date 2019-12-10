package ru.spbau.jvm.scala
package lecture06

object Main {

  import Appendable._

  def main(args: Array[String]): Unit = {
    val helloPrefix = "hello" :: 42 :: false :: HNil
    val worldSuffix = "world" :: HNil

    HNil.:::(HNil)(nilAppendable)
    worldSuffix.:::(HNil)(nilAppendable)
    worldSuffix.:::(helloPrefix)(appendable(appendable(appendable(nilAppendable))))

    val list = helloPrefix ::: worldSuffix
    val hello: String = list.head
    val world: String = list.tail.tail.tail.head
    println(s"$hello $world")
  }
}
