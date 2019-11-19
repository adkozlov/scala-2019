package ru.spbau.jvm.scala

class Table(private val tableNamesToIndices: Map[String, Int], private var table: List[List[FieldValue]]) {
  private val FieldDelimiter = " | "
  private val EndLine = "\n"

  def join(other: Table, onL: String, onR: String): Table = {
    val newTableToNameIndices = (tableNamesToIndices.toList ++
      other.tableNamesToIndices.map((x : (String, Int)) => (x._1, x._2 + tableNamesToIndices.size))).toMap

    val newTable = table.flatMap(lineL => other.table.map(
      lineR => if (lineL(tableNamesToIndices(onL)) == lineR(other.tableNamesToIndices(onR)))
        Some(lineL ++ lineR)
      else None).filter(_.isDefined).map(_.get))

    new Table(newTableToNameIndices, newTable)
  }
  def join(other: Table, on: String): Table = join(other, on, on)

  def groupBy(keyFields: List[String], by: String): Map[List[FieldValue], List[FieldValue]] = {
    table.map(row => (keyFields.map(key => row(tableNamesToIndices(key))), row(tableNamesToIndices(by)))).
      groupBy(_._1).map(x => (x._1, x._2.map(_._2)))
  }

  def filterRows(p: Map[String, FieldValue] => Boolean): Table = {
    val indicesToNames = tableNamesToIndices.toList.map(_.swap).toMap
    new Table(tableNamesToIndices,
      table.filter(x => p(x.zipWithIndex.map(_.swap).map(x => (indicesToNames(x._1), x._2)).toMap)))
  }

  def addColumn(name: String, column: List[FieldValue], index: Int): Table = {
    if (column.length != table.length) {
      throw new RuntimeException("Wrong number of elements in given column.")
    }

    val newTableToNameIndices = (name, index) ::
      tableNamesToIndices.toList.map((x : (String, Int)) => if (x._2 >= index) (x._1, x._2 + 1) else (x._1, x._2))

    new Table(newTableToNameIndices.toMap, table.zip(column).map(x => x._1.take(index) ++ List(x._2) ++ x._1.drop(index)))
  }
  def addColumn(name: String, column: List[FieldValue]): Table = addColumn(name, column, tableNamesToIndices.size)

  def getColumn(index: Int): List[FieldValue] = table.map(_.apply(index))
  def getColumn(name: String): List[FieldValue] = getColumn(tableNamesToIndices(name))

  def removeColumn(name: String): Table= removeColumn(tableNamesToIndices(name))
  def removeColumn(index: Int): Table = {
    if (index > tableNamesToIndices.size) {
      throw new RuntimeException("Column index out of bounds for this table.")
    }

    val newTableNamesToIndices = tableNamesToIndices.toList.
      filter(x => x._2 != index).map((x : (String, Int)) => if (x._2 >= index) (x._1, x._2 - 1) else (x._1, x._2))


    new Table(newTableNamesToIndices.toMap, table.map(x => x.take(index) ++ x.drop(index + 1)))
  }

  def print(): String =
    (tableNamesToIndices.toList.sortBy(_._2).map(_._1).mkString(FieldDelimiter) ::
      table.map(_.mkString(FieldDelimiter))).mkString(EndLine)
}

object Table {
  private val EndLine = '\n'
  private val FieldDelimiter = ';'

  private def readTable(filePath: String, readers: List[String => FieldValue]): List[List[FieldValue]] = {
    val source = scala.io.Source.fromFile(filePath)
    try
      List(source.mkString.split(EndLine).map(x =>
        List(x.split(FieldDelimiter).zip(readers).map(x => x._2(x._1)): _*)): _*)
    finally {
      source.close()
    }
  }

  def apply(filePath: String, columnNamesAndReaders: Schema.Fields): Table = {
    val (columnNames, readers) = columnNamesAndReaders.unzip
    val tableNamesToIndices = Map(columnNames.zipWithIndex.toMap.toSeq: _*)
    new Table(tableNamesToIndices, readTable(filePath, readers))
  }
}
