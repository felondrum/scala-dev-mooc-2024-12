package ru.otus.module3

import ru.otus.module3.zio_homework.config.{AppConfig, Configuration}
import zio.Clock.{ClockLive, currentTime}
import zio._
import zio.Console._
import zio.Random._

import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */


  lazy val guessProgram = for {
    target <- nextIntBetween(1, 4) // Генерируем число от 1 до 3
    _ <- printLine("Угадайте число от 1 до 3!")
    input <- readLine
    _ <- ZIO.fromOption(input.toIntOption)
      .orElseFail(new NumberFormatException("Введите число!"))
    _ <- if (input.toInt == target)
      printLine("Поздравляю! Вы угадали!")
    else
      printLine(s"Не угадали! Загаданное число было $target.")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = {
    def loop: ZIO[R, E, A] = effect.flatMap { result =>
      if (!condition(result)) loop
      else ZIO.succeed(result)
    }

    loop
  }


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */

  val defaultConfig: AppConfig = AppConfig(host = "localhost", port = "8080")

  def loadConfigOrDefault: RIO[Console, AppConfig] =
    Configuration.config
      .tapBoth(
        error => Console.printLineError(
          s"Ошибка чтения конфига: ${error.getMessage}\n" +
            s"Используются значения по умолчанию: $defaultConfig"
        ),
        config => Console.printLine(s"Успешно загружен конфиг: $config")
      )
      .orElseSucceed(defaultConfig)


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random, Int] =
    for {
      _ <- ZIO.sleep(1.second) // Задержка 1 секунда
      num <- nextIntBetween(0, 11) // Случайное число от 0 до 10 (включительно)
      _ <- Console.printLine(s"[FROM GENERATION EFFECT] Got number: ${num}").orDie
    } yield num

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: URIO[Random, List[Int]] = ZIO.collectAllPar(List.fill(10)(eff))


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  def printEffectRunningTimeAndGetSum[R, A](zio: URIO[R, List[Int]]): URIO[R with Clock with Console, Int] = for {
    startTime <- currentTime(TimeUnit.MILLISECONDS)
    numbers <- zio
    sum = numbers.sum
    endTime <- currentTime(TimeUnit.MILLISECONDS)
    _ <- Console.printLine(s"[FROM SUM EFFECT] Running time ${endTime - startTime}ms").orDie
    _ <- Console.printLine(s"[FROM SUM EFFECT] SUM: ${sum}").orDie
  } yield sum


  lazy val app: URIO[Random with Clock with Console, Int] = printEffectRunningTimeAndGetSum(effects)

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  //lazy val appSpeedUp = ???


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */
  object Profiler {
    trait Service {
      def printEffectRunningTime[R, A](zio: URIO[R, A]): URIO[R with Clock with Console, A]
    }

    val live: ULayer[Service] = ZLayer.succeed(new Service {
      def printEffectRunningTime[R, A](zio: URIO[R, A]): URIO[R with Clock with Console, A] =
        for {
          startTime <- currentTime(TimeUnit.MILLISECONDS)
          result <- zio
          endTime <- currentTime(TimeUnit.MILLISECONDS)
          _ <- Console.printLine(s"[FROM PROFILER EFFECT] Effect running time: ${endTime - startTime}ms").orDie
        } yield result
    })

    // Доступные методы для пользователей сервиса
    def printEffectRunningTime[R, A](zio: URIO[R, A]): URIO[R with Clock with Console with Service, A] =
      ZIO.serviceWithZIO[Service](_.printEffectRunningTime(zio))
  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы создать эффект,
   * который будет логировать время выполнения программы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: URIO[Random with Clock with Console with Profiler.Service, Int]
  = Profiler.printEffectRunningTime(app)

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */
  lazy val runApp = {
    appWithTimeLogg.provide(
      ZLayer.succeed(RandomLive),
      ZLayer.succeed(ClockLive),
      ZLayer.succeed(ConsoleLive),
      Profiler.live)
  }
}

