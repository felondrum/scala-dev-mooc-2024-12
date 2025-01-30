package ru.otus.module1.DataCollection1

import scala.util.Random

class BallsExperiment {

  val ball: Boolean = isFirstBlackSecondWhite()
  def isFirstBlackSecondWhite(): Boolean = {
    Random.nextBoolean()
