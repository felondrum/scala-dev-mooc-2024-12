package ru.otus.module1.DataCollection1

import scala.util.Random

class BallsExperiment {

  val ball: Boolean = isFirstBlackSecondWhite()
  def isFirstBlackSecondWhite(): Boolean = {
    Random.nextBoolean()
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] =
      Range(0, count)
        .map(_ => new BallsExperiment()).toList
    val countOfExperiments = listOfExperiments.filter(b => b.ball).map(b => b.ball)
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}
