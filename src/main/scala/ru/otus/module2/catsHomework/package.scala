package ru.otus.module2

import cats.Functor
import ru.otus.module2.catsHomework.{Branch, Leaf, Tree, treeFunctor}

import scala.util.{Failure, Success, Try}

object d {
  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val result: Tree[Int] = treeFunctor.map(tree)(_ + 3)
    println(tree) //Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))
    println(result) //Branch(Leaf(4),Branch(Leaf(5),Leaf(6)))
  }
}

package object catsHomework {


  /**
   * Простое бинарное дерево
   *
   * @tparam A
   */
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

  lazy val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }

  // Вывод: Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))

  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   *
   * @tparam F
   */
  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def pure[A](v: A): F[A]
  }


  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   *
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F] {
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

  lazy val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {

    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    def pure[A](v: A): Try[A] = Success(v)

    def raiseError[A](e: Throwable): Try[A] = Failure(e)

    def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa.recoverWith { case e => f(e) }

    def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa.recover { case e => f(e) }

    def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = fa.filter(f).recoverWith { case _ => Failure(e) }
  }

  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */

  //не компилируется. Не могу понять почему
  val eitherME: MonadError[Either[String, Any], String] = new MonadError[Either[String, Any], String] {
    def flatMap[A, B](fa: Either[String, A])(f: A => Either[String, B]): Either[String, B] = fa.flatMap(f)

    def pure[A](v: A): Either[String, A] = Right(v)

    def raiseError[A](e: String): Either[String, A] = Left(e)

    def handleErrorWith[A](fa: Either[String, A])(f: String => Either[String, A]): Either[String, A] = {
      fa match {
        case Left(e) => f(e)
        case Right(v) => Right(v)
      }
    }

    def handleError[A](fa: Either[String, A])(f: String => A): Either[String, A] = {
      fa match {
        case Left(e) => Right(f(e))
        case Right(v) => Right(v)
      }
    }

    def ensure[A](fa: Either[String, A])(e: String)(f: A => Boolean): Either[String, A] = {
      fa match {
        case Right(v) if f(v) => Right(v)
        case Right(_) => Left(e)
        case Left(e) => Left(e)
      }
    }

  }

}
