package ru.spbau.jvm.scala.heterogeneous

object Main {

  def main(args: Array[String]): Unit = {
    val helloPrefix = "hello" :: HNil
    val worldSuffix = 5 :: HNil
    val result = helloPrefix.zip(worldSuffix)
    println(result)
    println(result.splitAt(Succ(Zero)))
  }
}
