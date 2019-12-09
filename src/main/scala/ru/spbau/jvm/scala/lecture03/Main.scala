package ru.spbau.jvm.scala
package lecture03

object Main {

  def main(args: Array[String]): Unit = {
    val list: List[Int] = 1 :: 2 :: 3 :: Nil // Nil.::(3).::(2).::(1)

    list.map(inc1)
      .foreach(println)

    println(42 +: list :+ 24)
    println(list ::: (4 :: 5 :: 6 :: Nil))

    import lecture01._
    val bars: List[Bar] = (new Bar(0)) :: Nil
    val foos: List[Foo] = bars
  }

  val IncFunction: Int => Int = new Function1[Int, Int] {
    override def apply(value: Int): Int = value + 1
  }

  val IncPartialFunction: Int => Int = new PartialFunction[Int, Int] {
    override def isDefinedAt(value: Int): Boolean = true

    override def apply(value: Int): Int =
      if (value == 0) 1 else value + 1
  }

  def inc1(int: Int): Int = int + 1

  /**
   * @see [[IncFunction]]
   */
  def inc2: Int => Int = int => int + 1

  /**
   * @see [[IncFunction]]
   */
  def inc3: Int => Int = _ + 1

  /**
   * @see [[IncFunction]]
   */
  def inc4: Int => Int = (_: Int) + 1

  /**
   * @see [[IncPartialFunction]]
   */
  def inc5: Int => Int = {
    case 0 => 1
    case int => int + 1
  }
}
