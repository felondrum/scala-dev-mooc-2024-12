package ru.otus.module1.DataCollection2

object MonadaRules {

  def sqrt(x:Int): Option[Int] = Some(x*x)
  def inc(x: Int): Option[Int] = Some(x+x)

  //left unit law
  // unit(x).flatMap(f) == f(x)
  def leftUnitLaw(): Unit = {
    val x = 5
    val monad: Option[Int] = Some(x)
    val result = monad.flatMap(sqrt) == sqrt(x)
    println(result)
  }

  // right unit law
  // m.flatMap(unit) == m
  def rightUnitLaw(): Unit = {
    val x = 5
    val monad:Option[Int] = Some(x)
    val result = monad.flatMap(x=>Some(x)) == monad
    println(result)
  }

  //associative law
  //m.flatMap(f).flatMap(g) == m.flatMap(x=>f(x).flatMap(g))
  def associativeLaw(): Unit = {
    val x = 5
    val monad:Option[Int] = Some(x)

    val left = monad flatMap sqrt flatMap inc
    val right = monad flatMap(x=> sqrt(x) flatMap inc)
    assert(left == right)
  }

  def main(args: Array[String]): Unit ={
    leftUnitLaw
    rightUnitLaw
    associativeLaw
  }

}
