package ru.spbau.jvm.scala.multiset

abstract class Collection[K] {
  def size(): Int

  def isEmpty: Boolean = (size == 0)

  def contains(key: K): Boolean

  def add(key: K): Unit

  def remove(key: K): Unit

  def clear(): Unit
}

abstract class Iterator[K] {
  def hasNext: Boolean

  def next(): K
}