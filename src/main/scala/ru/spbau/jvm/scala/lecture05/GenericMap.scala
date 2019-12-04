package ru.spbau.jvm.scala.lecture05

final class GenericMap[K <: AnyVal, V >: Null] {

  final case class Entry(key: K = 42,
                         value: V = "")

  def put(entry: GenericMap[K, V]#Entry /* type projection */): this.type = this

  def apply(key: K): Option[V] = None
}
