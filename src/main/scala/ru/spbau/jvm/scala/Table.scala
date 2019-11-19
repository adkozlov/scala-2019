package ru.spbau.jvm.scala

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Table(private var tableNamesToIndices: mutable.Map[String, Int], private var table: ListBuffer[ListBuffer[FieldValue]]) {
  val FieldDelimiter = " | "
  val EndLine = "\n"

  def join(other: Table, onL: String, onR: String): Table = {
    def nub(listBuffer: ListBuffer[FieldValue]) = {
      if (onL == onR) {
        listBuffer.remove(tableNamesToIndices(onL))
      }

      listBuffer
    }

    val newTableToNameIndices = tableNamesToIndices.clone().
      addAll(other.tableNamesToIndices.map((x : (String, Int)) => (x._1, x._2 + tableNamesToIndices.size)))

    val newTable = table.flatMap(lineL => other.table.map(
      lineR => if (lineL(tableNamesToIndices(onL)) == lineR(other.tableNamesToIndices(onR)))
        Some(nub(lineL ++ lineR))
      else None).filter(_.isDefined).map(_.get))

    new Table(newTableToNameIndices, newTable)
  }
  def join(other: Table, on: String): Table = join(other, on, on)

  def filterRows(p: Map[String, FieldValue] => Boolean): Table = {
    val indicesToNames = tableNamesToIndices.toList.map(_.swap).toMap
    table.filterInPlace(x => p(x.zipWithIndex.map(_.swap).map(x => (indicesToNames(x._1), x._2)).toMap))
    this
  }

  def addColumn(name: String, column: List[FieldValue], index: Int): Table = {
    if (column.length != table.length) {
      throw new RuntimeException("Wrong number of elements in given column.")
    }

    tableNamesToIndices.mapValuesInPlace((_, v) => if (v >= index) v + 1 else v)
    tableNamesToIndices.addOne((name, index))

    table.zip(column).foreach(x => x._1.insert(index, x._2))

    this
  }
  def addColumn(name: String, column: List[FieldValue]): Table = addColumn(name, column, tableNamesToIndices.size)

  def getColumn(index: Int): List[FieldValue] = table.map(_.apply(index)).toList
  def getColumn(name: String): List[FieldValue] = getColumn(tableNamesToIndices(name))

  def removeColumn(name: String): Table= removeColumn(tableNamesToIndices(name))
  def removeColumn(index: Int): Table = {
    if (index > tableNamesToIndices.size) {
      throw new RuntimeException("Column index out of bounds for this table.")
    }

    tableNamesToIndices.remove(tableNamesToIndices.find(x => x._2 == index).get._1)
    tableNamesToIndices.mapValuesInPlace((_, v) => if (v >= index) v - 1 else v)

    table.map(_.remove(index))

    this
  }

  def print(): String =
    (tableNamesToIndices.toList.sortBy(_._2).map(_._1).mkString(FieldDelimiter) ::
      table.map(_.mkString(FieldDelimiter)).toList).mkString(EndLine)
}

object Table {
  private val EndLine = '\n'
  private val FieldDelimiter = ';'

  private def readTable(filePath: String, readers: List[String => FieldValue]): ListBuffer[ListBuffer[FieldValue]] = {
    val source = scala.io.Source.fromFile(filePath)
    try
      ListBuffer(source.mkString.split(EndLine).map(x =>
        ListBuffer(x.split(FieldDelimiter).zip(readers).map(x => x._2(x._1)): _*)): _*)
    finally {
      source.close()
    }
  }

  def apply(filePath: String, columnNamesAndReaders: Schema.Fields): Table = {
    val (columnNames, readers) = columnNamesAndReaders.unzip
    val tableNamesToIndices = mutable.Map(columnNames.zipWithIndex.toMap.toSeq: _*)
    new Table(tableNamesToIndices, readTable(filePath, readers))
  }
}
