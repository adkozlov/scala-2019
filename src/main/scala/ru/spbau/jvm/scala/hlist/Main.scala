package ru.spbau.jvm.scala.hlist

object Main {

  import Appendable._

  def main(args: Array[String]): Unit = {
    val helloPrefix = "hello" :: 42 :: false :: HNil
    val worldSuffix = "world" :: true :: HNil

    HNil.:::(HNil)(nilAppendable)
    worldSuffix.:::(HNil)(nilAppendable)
    worldSuffix.:::(helloPrefix)(appendable(appendable(appendable(nilAppendable))))

    val list = helloPrefix ::: worldSuffix
    val hello: String = list.head
    val world: String = list.tail.tail.tail.head

    val zippedList = helloPrefix zip worldSuffix
    println(zippedList.head)
    println(zippedList.tail.head)

    val q = "h"  :: "w" :: HNil
    println(q splitAt Succ(Zero))
    println(s"$hello $world")
  }
}
