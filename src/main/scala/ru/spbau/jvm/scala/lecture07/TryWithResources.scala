package ru.spbau.jvm.scala
package lecture07

import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException}

object TryWithResources {

  def main(args: Array[String]): Unit = args match {
    case Array(head) =>
      withClose(new BufferedReader(new FileReader(head))) { reader =>
        var line = reader.readLine()
        while (line != null) {
          println(line)
          line = reader.readLine()
        }
      } {
        case e: FileNotFoundException =>
          println(e.getMessage)
        case e: IOException =>
          for {
            exception <- e +: e.getSuppressed
            message = exception.getMessage
          } println(message)
      }
    case _ =>
  }
}