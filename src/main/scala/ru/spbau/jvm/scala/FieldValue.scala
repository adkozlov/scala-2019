package ru.spbau.jvm.scala

sealed trait FieldValue {
  def asInt: Int = throw new UnsupportedOperationException("FieldValue can't be converted to Int")
  def asString: String = throw new UnsupportedOperationException("FieldValue can't be converted to String")
  def asFloat: Float = throw new UnsupportedOperationException("FieldValue can't be converted to Float")
  def asLong: Long = throw new UnsupportedOperationException("FieldValue can't be converted to Long")
}

case class IntValue(value: Int) extends FieldValue {
  override def asInt: Int = value
  override def asFloat: Float = value

  override def toString: String = asInt.toString
}

case class StringValue(value: String) extends FieldValue {
  override def asString: String = value

  override def toString: String = asString.toString
}

case class FloatValue(value: Float) extends FieldValue {
  override def asFloat: Float = value

  override def toString: String = asFloat.toString
}

case class LongValue(value: Long) extends FieldValue {
  override def asLong: Long = value
  override def asFloat: Float = value

  override def toString: String = asLong.toString
}

object FieldValue {
  def readIntFieldValue(line: String): FieldValue = IntValue(line.toInt)
  def readStringFieldValue(line: String): FieldValue = StringValue(line)
  def readFloatFieldValue(line: String): FieldValue = FloatValue(line.toFloat)
  def readLongFieldValue(line: String): FieldValue = LongValue(line.toLong)
}
