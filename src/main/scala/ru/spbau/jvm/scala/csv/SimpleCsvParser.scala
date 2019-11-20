package ru.spbau.jvm.scala.csv

import scala.util.{Failure, Success, Try}

class SimpleCsvParser(separator: Char) {
  def parse(inputIterator: Iterator[String]): Try[SimpleCsv] = {
    val header: List[String] = inputIterator.next().split(separator).toList
    val content = inputIterator.map { x => x.split(separator).toList }.toList
    validateContext(header.length, content) match {
      case Some(error) => Failure(error)
      case None => Success(SimpleCsv(header, content))
    }
  }

  private def validateContext(expectedLen: Int, content: List[List[String]]): Option[SimpleCsvParserFormatException] = {
    val badIndex = content.indexWhere(p => p.length != expectedLen)
    if (badIndex >= 0)
      Some(new SimpleCsvParserFormatException(badIndex))
    else
      None
  }
}
