package ru.spbau.jvm.scala
package lecture04

object Main {

  import lecture03._

  def main(args: Array[String]): Unit = {
    println {
      // for comprehensions
      for {
        arg <- args //.toSeq
        concat = arg + arg

        char <- concat
        if char != 'l'
      } yield char
    }

    println {
      args //.toSeq
        .map(arg => (arg, arg + arg))
        .flatMap(_._2)
        .filter(_ != 'l')
    }

    val filteredList = for {
      i <- 1 :: 2 :: 0 :: 3 :: Nil
      if i != 0 // withFilter is an extension method
    } yield i

    println(filteredList.length)

    val token = libA.Token(args(0))
    onToken(token)
  }

  private def onToken(token: libB.Token): Unit = println(token.text)
}
