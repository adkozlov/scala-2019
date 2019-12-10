package ru.spbau.jvm.scala.lecture05

// please use main method instead
object Main extends App {

  {
    val firstMap = new TypeMemberMap {
      override type K = Int
      override type V = String
    }

    val secondMap = new TypeMemberMap {
      override type K = Int
      override type V = String
    }

    firstMap.put(firstMap.Entry(42, ""))
    //  firstMap.put(secondMap.Entry(42, "")) // compilation error (path mismatch)
  }

  {
    val firstMap: IntToString = new GenericMap[Int, String]
    val secondMap: IntToString = new GenericMap[Int, String]

    firstMap.put(firstMap.Entry(42, ""))
    firstMap.put(secondMap.Entry(42, "")) // no compilation error
  }
}
