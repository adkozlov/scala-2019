package ru.spbau.jvm.scala

import scala.collection.mutable.ListBuffer
import scala.io.Source

trait TableEntity

trait Table[Entity <: TableEntity] {

  protected val SEPARATOR: String = ","

  protected var entities: ListBuffer[Entity] = ListBuffer[Entity]()

  protected def parseEntity(file: String): Entity

  def load(filename: String): List[Entity] = {
    val file = Source.fromFile(filename)
    file.getLines().foreach { line =>
      entities.addOne(parseEntity(line))
    }
    file.close()
    entities.toList
  }

  def getEntities: List[Entity] = entities.toList
}
