package ru.spbau.jvm.scala

case class EmployeeTotal(employee: Employee,
                         var duration: Int,
                         var cost: Float) {
  def +(other: EmployeeTotal): EmployeeTotal = {
    if (employee != other.employee) {
      throw new IllegalArgumentException
    }
    EmployeeTotal(employee, duration + other.duration, cost + other.cost)
  }
}
