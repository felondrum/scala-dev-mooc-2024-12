package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правом результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    //  task"Реализуйте метод `fullSequence`"()

    // Изначально написал как в блоке (1) - пока сложно без явно указанных типов.
    // Уже после переписано в более функциональном стиле

    //(1) - изначальный вариант
    //    val futureToEither: Future[A] => Future[Either[Throwable, A]] = {
    //      future =>
    //        future.transform {
    //          case Success(value) => Success(Right(value))
    //          case Failure(exception) => Success(Left(exception))
    //        }
    //    }
    //
    //    val eitherListToTuple: List[Either[Throwable, A]] => (List[A], List[Throwable]) = {
    //      listOfEither =>
    //        val (successes, failed): (List[Either[Throwable, A]], List[Either[Throwable, A]]) =
    //          listOfEither.partition(_.isRight)
    //        val successList: List[A] = successes.collect { case Right(value) => value }
    //        val failedList: List[Throwable] = failed.collect { case Left(exception) => exception }
    //        (successList, failedList)
    //    }
    //
    //    Future.traverse(futures)(futureToEither).map(eitherListToTuple)
    //

    //(2) - итог
    Future.traverse(futures) {
      future =>
        future.transform {
          case Success(value) => Success(Right(value))
          case Failure(exception) => Success(Left(exception))
        }
    }.map {
      listOfEither =>
        val (successes, failed) = listOfEither.partition(_.isRight)
        (successes.collect { case Right(value) => value },
          failed.collect { case Left(exception) => exception })
    }

  }

}
