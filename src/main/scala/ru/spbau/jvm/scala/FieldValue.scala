package ru.spbau.jvm.scala

sealed trait FieldValue {
  def toInt: Int = throw new UnsupportedOperationException("FieldValue can't be converted to Int")
  override def toString: String = throw new UnsupportedOperationException("FieldValue can't be converted to String")
  def toFloat: Float = throw new UnsupportedOperationException("FieldValue can't be converted to Float")
  def toLong: Long = throw new UnsupportedOperationException("FieldValue can't be converted to Long")
}

case class IntValue(value: Int) extends FieldValue {
  override def toInt: Int = value
}

case class StringValue(value: String) extends FieldValue {
  override def toString: String = value
}

case class FloatValue(value: Float) extends FieldValue {
  override def toFloat: Float = value
}

case class LongValue(value: Long) extends FieldValue {
  override def toLong: Long = value
}

object FieldValue {
  def readIntFieldValue(line: String): FieldValue = IntValue(line.toInt)
  def readStringFieldValue(line: String): FieldValue = StringValue(line)
  def readFloatFieldValue(line: String): FieldValue = FloatValue(line.toFloat)
  def readLongFieldValue(line: String): FieldValue = LongValue(line.toLong)
}
