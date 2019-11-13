package ru.spbau.jvm.scala.lecture01

final class Bar(myFoo: Int) extends Foo {

  override def foo = myFoo

  override def toString = s"Bar($myFoo)"

  override def hashCode() = 31 * myFoo

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Bar]) return false

    val bar = obj.asInstanceOf[Bar]
    // bar.myFoo == myFoo // compilation error (myFoo is not a member)
    bar.foo == foo
  }
}

object Bar {
  def apply(): Bar =
    apply(42)

  private def apply(foo: Int) =
    new Bar(foo)
}