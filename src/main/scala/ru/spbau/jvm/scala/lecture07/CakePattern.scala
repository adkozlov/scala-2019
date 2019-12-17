package ru.spbau.jvm.scala
package lecture07

object CakePattern {

  trait FooComponent {

    val foo: Foo

    protected trait Foo {
      def foo: String
    }

  }

  trait FooComponentImpl extends FooComponent {
    override val foo: Foo = new FooImpl

    private class FooImpl extends Foo {
      override def foo: String = "foo"
    }

  }

  trait BarComponent {

    val bar: Bar

    protected trait Bar {
      def bar: String
    }

  }

  trait BarComponentImpl extends BarComponent {
    override val bar: Bar = new BarImpl

    private class BarImpl extends Bar {
      override def bar: String = "bar"
    }

  }

  trait Baz {

    this: FooComponent with BarComponent => // self: FooComponent with BarComponent =>

    def baz: String = foo.foo + bar.bar
  }

  def main(args: Array[String]): Unit = {
    val baz = new Baz with FooComponentImpl with BarComponentImpl
    println(baz.baz)
  }
}