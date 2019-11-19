package ru.spbau.jvm.scala.db.tables.rows

trait Row {
  var data: List[Any] = List[Any]()

  def getElem(i: Int): String = {
    data(i).toString
  }

  def getArrStrRow(): List[String] = {
    data.map(x => x.toString)
  }
}