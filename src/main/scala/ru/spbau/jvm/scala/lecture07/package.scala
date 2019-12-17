package ru.spbau.jvm.scala

package object lecture07 {

  def withClose[C <: AutoCloseable, R](init: => C)
                                      (action: C => R)
                                      (recover: PartialFunction[Throwable, R]): R = {
    var closeable: C = null.asInstanceOf[C]
    try {
      closeable = init
      action(closeable)
    } catch {
      recover
    } finally {
      if (closeable != null) {
        try {
          closeable.close()
        } catch {
          case _: Exception =>
        }
      }
    }
  }

}