package ru.spbau.jvm
package scala.lecture05

abstract class TypeMemberMap {

  type K <: AnyVal
  type V >: Null

  final case class Entry(key: K, value: V)

  def put(entry: Entry): this.type = this

  def apply(key: K): Option[V] = None
}
