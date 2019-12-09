package ru.spbau.jvm.scala.multiset

trait Tree[K, V >: Null] {
  def add(key: K, elem: V)

  def get(key: K): V

  def remove(key: K)

  def changeValue(key: K, newValue: V)

  def size(): Int

  def toList: List[(K, V)]

  def iterator(): Iterator[(K, V)]
}
