package ru.spbau.jvm.scala
package lecture06

object Main {

  def main(args: Array[String]): Unit = {
    val helloPrefix = "hello" :: 42 :: false :: HNil
    val worldSuffix = "world" :: HNil

    println(worldSuffix zip HNil)
    println(HNil zip worldSuffix)
    println(worldSuffix zip 1 :: HNil)
    println(worldSuffix zip 1 :: false :: HNil)
    println(worldSuffix ::: 1 :: HNil zip 1 :: false :: HNil)

    println(helloPrefix splitAt Zero)
    println(helloPrefix splitAt Succ(Zero))
    println(helloPrefix splitAt Succ(Succ(Zero)))
    println(helloPrefix splitAt Succ(Succ(Succ(Zero))))
    //        helloPrefix splitAt Succ(Succ(Succ(Succ(Zero))))

    val list = helloPrefix ::: worldSuffix
    val hello: String = list.head
    val world: String = list.tail.tail.tail.head
    println(s"$hello $world")
  }
}
