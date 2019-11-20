package ru.spbau.jvm.scala.database


class DbFileParserFormatException(private val str: String) extends Exception {
  override def getMessage: String = s"malformed attribute header: `$str`"
}

class DbFileParserMalformedNameException(private val str: String) extends Exception {
  override def getMessage: String = s"malformed table name: `$str`"
}

class DbNoSuchColumnException(column: String) extends Exception {
  override def getMessage: String = s"no such column $column"
}

class DbNoSuchTableException(table: String) extends Exception {
  override def getMessage: String = s"no such table $table"
}

class DbSelectException(column: String) extends Exception {
  override def getMessage: String = s"unable to select a value from column $column"
}
