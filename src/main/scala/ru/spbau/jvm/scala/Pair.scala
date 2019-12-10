package ru.spbau.jvm.scala

sealed class Pair[K, V](val key: K, val value: V)

object Pair {
  def apply[K, V](key: K, value: V): Pair[K, V] = new Pair(key, value)
}
