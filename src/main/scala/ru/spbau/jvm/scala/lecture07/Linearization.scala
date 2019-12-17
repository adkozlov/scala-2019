package ru.spbau.jvm.scala
package lecture07

object Linearization {

  trait Foo {
    def foo = "1"
  }

  trait Bar extends Foo {
    override def foo: String = "2" + super.foo
  }

  trait Baz extends Bar {
    override def foo: String = "3" + super.foo
  }

  trait BarBaz extends Foo {
    override def foo: String = "4" + super.foo
  }

  def main(args: Array[String]): Unit = {
    val foo = new Foo with BarBaz with Baz with Bar {
      override def foo: String = super.foo.reverse
    }
    println(foo.foo) // 1423
  }

}