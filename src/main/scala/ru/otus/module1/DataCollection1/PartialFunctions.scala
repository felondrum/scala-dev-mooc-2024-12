package ru.otus.module1.DataCollection1

object PartialFunctions extends App {
  val collection = List("sdf", 14, 15,3,9, "sdgfsdg")


  val eventNumberToString: PartialFunction[Any, String] ={
    case x if x.isInstanceOf[Int] && x.asInstanceOf[Int] % 2 == 0 => s"$x is even"
  }

  println(eventNumberToString.isDefinedAt(2)) // true
  println(eventNumberToString.isDefinedAt(3)) // false

  if (eventNumberToString.isDefinedAt(4)) {
    println(eventNumberToString(4))
  }

  val numberToString: PartialFunction[Any, String] = eventNumberToString.orElse{
    case x if x.isInstanceOf[String] => s"$x is String"
    case x => s"$x is odd"
  }

  collection.collect(numberToString).foreach(println)

}