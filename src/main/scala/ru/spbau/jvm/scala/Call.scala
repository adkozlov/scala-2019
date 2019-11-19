package ru.spbau.jvm.scala

import java.util.Date


case class Call(user : User, callee : String, duration : Int, cost : Double, date : Date)
