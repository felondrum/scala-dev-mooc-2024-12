package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(x: Int, accum: Int): Int = {
      if( n <= 0) accum
      else loop(x - 1, x * accum)
    }
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции


  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  def sum(x: Int, y: Int): Int = x + y


  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 5



















}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal
  class Cat extends Animal

  def treat(animal: Animal): Unit = ???
  def treat(animal: Option[Animal]): Unit = ???

  val d: Dog = ???
  val dOpt: Option[Dog] = ???
  treat(d)
  treat(dOpt)

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Variance
  // 1. Invariance
  // 2. Covariance
  // 3. Contrvariance

  // 1. invariance
  // 2. covariance  A <- B  Option[A] <- Option[B]
  // 3. contravariance A <- B Option[A] -> Option[B]
  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty option")
    }

    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = {
      if (isEmpty) println("None") else println(this.get)
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[T1 >: T, B](another: Option[B]): Option[(T1, B)] = {
      if (isEmpty || another.isEmpty) None else Some((this.get, another.get))
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */

    def filter(predicate: T => Boolean): Option[T] = {
      if (this.isEmpty || predicate.apply(this.get)) this else None
    }
  }

  case class Some[V](v: V) extends Option[V]

  case object None extends Option[Nothing] // Any <- Dog

  object Option {
    def apply[T](v: T): Option[T] =
      if (v == null) None
      else Some(v)
  }

  val o1: Option[Int] = Option(1)
  o1.isEmpty // false

}

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */

  trait List[+T] {
    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием`::`
     *
     */
    def ::[TT >: T](elem: TT): List[TT] = new List.::[TT](elem, this)

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */
    def mkString(delimiter: String): Unit = {
      @tailrec
      def loop(delimiter: String, acc: String, list: List[T]): String = {
        list match {
          case List.::(head, tail) =>
            if (acc.isBlank)
              loop(delimiter, acc + head, tail)
            else
              loop(delimiter, acc + delimiter + head, tail)
          case List.Nil => acc
        }
      }

      val emptyString = ""
      println(loop(delimiter, emptyString, this))
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse(): List[T] = {
      @tailrec
      def loop(list: List[T], acc: List[T]): List[T] = {
        list match {
          case List.::(head, tail) => {
            val acc_ = acc.::(head)
            loop(tail, acc_)
          }
          case List.Nil => acc
        }
      }

      loop(this, List.Nil)
    }

    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[TT >: T](f: TT => TT): List[TT] = {
      @tailrec
      def loop(list: List[TT], f: TT => TT, resultList: List[TT]): List[TT] = {
        list match {
          case List.Nil => resultList
          case List.::(head, tail) => {
            val res_ = resultList.::(f(head))
            loop(tail, f, res_)
          }
        }
      }

      val m_ = loop(this, f, List.Nil)
      m_.reverse()
    }

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */

    def filter(predicate: T => Boolean) : List[T] = {
      @tailrec
      def loop(list: List[T], predicate: T => Boolean, resultList: List[T]): List[T] = {
        list match {
          case List.Nil => resultList
          case List.:: (head, tail) => {
            val pr_ = predicate.apply(head)
            val res_ = if (!pr_) resultList.::(head) else resultList
            loop(tail, predicate, res_)
          }
        }
      }
      val m_ = loop(this, predicate, List.Nil)
      m_.reverse()
    }

  }

  object List {
    case class ::[A](head: A, tail: List[A]) extends List[A]

    case object Nil extends List[Nothing]

    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
    def apply[A](v: A*): List[A] =
      if (v.isEmpty) List.Nil else new::(v.head, apply(v.tail: _*))
  }

  val l1: List[Int] = List(1, 2, 3)
  val l2: List[Int] = 1 :: 2 :: 3 :: List.Nil




  /**
   *
   * Написать функцию incList, которая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */

  def incList(list: List[Int]): List[Int] = {
    list.map(x => x + 1)
  }


  /**
   *
   * Написать функцию shoutString которая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

  def shoutString(list: List[String]) : List[String] = {
    list.map(x => x.concat("!"))
  }

}