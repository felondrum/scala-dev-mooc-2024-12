package ru.otus.module1.DataCollection2

import scala.reflect.ClassTag

class MonadParser[T, Src](private val p: Src => (T, Src)) {
  def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      val mn = f(word)
      val res = mn.p(rest)
      res
    }

  def map[M](f: T => M): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }

  def parse(src: Src): T = p(src)._1
}

object MonadParser {
  def apply[T, Src](f: Src => (T, Src)) = new MonadParser[T, Src](f)

  def StringField(delimiter: Delimiter): MonadParser[String, String] = MonadParser[String, String] {
    str =>
      val idx = str.indexOf(delimiter)
      if (idx > -1)
        (str.substring(0, idx), str.substring(idx + 1))
      else
        (str, "")
  }

  def IntField(delimiter: Delimiter): MonadParser[Int, String] = StringField(delimiter).map(_.toInt)

  def BooleanField(delimiter: Delimiter): MonadParser[Boolean, String] = StringField(delimiter).map(_.toBoolean)

}

case class Car(year: Int, mark: String, model: String, canDrive: Boolean)

type Delimiter = String
type Splitter = String


object TestExecutor {
  def main(args: Array[String]): Unit = {
    val str = "1997;Ford;Passat;true\n1901;Ford;T;false"

    val delimiter: Delimiter = ";"

    given parser: MonadParser[Car, String] = for {
      year <- MonadParser.IntField(delimiter)
      mark <- MonadParser.StringField(delimiter)
      model <- MonadParser.StringField(delimiter)
      canDrive <- MonadParser.BooleanField(delimiter)
    } yield Car(year, mark, model, canDrive)

    given splitter: Splitter = "\n"

    def f(s: String)(using splitter: Splitter): Array[String] = s.split(splitter)

    val result: Array[Car] = ParserWithGivenParam(str, f).parseToArray

    println(result.mkString("; "))
  }
}


class ParserWithGivenParam[T, Src](raw: Src, splitF: Src => Array[Src])
                                  (using parser: MonadParser[T, Src], c: ClassTag[T]) {
  def parseToArray: Array[T] = {
    splitF(raw).map(parser.parse)
  }

}

object ParserWithGivenParam {
  def apply[T, Src](raw: Src, splitF: Src => Array[Src])
                   (using parser: MonadParser[T, Src], c: ClassTag[T])
  = new ParserWithGivenParam(raw, splitF)

}

