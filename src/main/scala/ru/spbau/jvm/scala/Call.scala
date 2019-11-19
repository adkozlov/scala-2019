package ru.spbau.jvm.scala

class Call(
      val firstName: String,
      val secondName: String,
      val number: String,
      val cost: Float,
      val duration: Float,
      val date: String
) {
    override def toString: String = {
        firstName + " " +
        secondName + " " +
        number + " " +
        cost + " " +
        duration + " " +
        date
    }
}
