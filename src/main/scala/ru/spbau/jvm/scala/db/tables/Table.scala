package ru.spbau.jvm.scala.db.tables

import java.io.File

import ru.spbau.jvm.scala.db.tables.rows.{Row, RowFabric}

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

trait Table[_ <: Row] {
  private var title: List[String] = List[String]()
  private var body: ListBuffer[Row] = ListBuffer[Row]()

  def load(fileName: File): Unit

  def loadWithRowFabric(fileName: File, rowFabric: RowFabric): Unit = {
    val bufferedSource: BufferedSource = io.Source.fromFile(fileName)
    val data = bufferedSource.getLines().splitAt(1)

    title = data._1.next().split(",").toList

    data._2.foreach { line =>
      body += rowFabric.create(line.split(","))
    }
  }

  def getActionTable(): ActionTable = {
    var resBody: ListBuffer[List[String]] = ListBuffer[List[String]]()
    body.foreach(row => resBody += row.getArrStrRow())
    new ActionTable(title, resBody)
  }
}
