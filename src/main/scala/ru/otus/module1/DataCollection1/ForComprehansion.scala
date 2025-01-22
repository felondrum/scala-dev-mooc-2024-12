package ru.otus.module1.DataCollection1


object ForComprehansion {
  def main(args: Array[String]): Unit = {
    val RGB = Seq("R", "G", "B")
    val range = Range(1,4)
    val map = Map("R"-> "Rot", "G"-> "Grün", "B"-> "Blau")

    for (el <- RGB)
      println(el)


    for (el <- RGB; el1 <- range) {
      println(s"$el $el1")
    }

    println("----------------")
    for (
      el1 <- RGB;
      el2 <- RGB;
      el3 <- RGB
    ) {
      println(s"$el1 $el2 $el3")
    }

    println("make it uniq")
    for (
      el1 <- RGB;
      el2 <- RGB;
      el3 <- RGB;
      if el1 != el2;
      if el3 != el2 && el3 != el1
    ){
      println(s"$el1 $el2 $el3")
    }


    println("next example")
    case class Car(marke: String, model: String, year: Int)
    val cars = Car("VW", "Passat", 2005) :: Car("Lexus", "UX", 2019) :: Car("BMW", "i3", 2021) :: Nil
    case class Garage(name: String, index: Int)
    val garages = Garage("BMW", 1) :: Garage("Lexus", 2) :: Nil

    garages.flatMap{
      garage =>
        cars.filter(car => car.marke == garage.name).map(car => (car.marke, garage.index))
    }.foreach(x=> println(s"${x._1} ${x._2}"))

    println("--------------------------")

    val cars1 = for {
      car <- cars
      garage <- garages
      if car.marke == garage.name
    } yield {
      (car.marke, garage.index)
    }

    cars1.foreach({
      case (marke, index) => println(s"$marke $index")
    })



  }
}